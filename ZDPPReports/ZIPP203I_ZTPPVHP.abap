************************************************************************
* Program Name      : ZIPP203I_ZTPPVHP
* Author            : DongYeop, Han
* Creation Date     : 2003.11.12.
* Specifications By : DongYeop, Han
* Pattern           : 1.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Transfer of Production Spec from PP to ALC
*                     (ztppvhp)
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zipp203i_ztppvhp   NO STANDARD PAGE HEADING
                           LINE-SIZE 1023
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

DATA : it_wohd      LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
*      it_wocl      LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
       it_zsppvhp   LIKE TABLE OF zsppvhp        WITH HEADER LINE,
       it_specvs    LIKE TABLE OF ztppvs         WITH HEADER LINE,
       it_ztppvhp   LIKE TABLE OF ztppvhp        WITH HEADER LINE,
       it_zsppvhp1  LIKE TABLE OF zsppvhp1       WITH HEADER LINE,
       it_ztppvhp1  LIKE TABLE OF ztppvhp1       WITH HEADER LINE.
DATA : BEGIN OF it_message_p OCCURS 0,
       funcname(20),
       msgtxt(100).
DATA : END OF it_message_p.

DATA: BEGIN OF it_status OCCURS 0,
        fcode LIKE rsmpe-func,
      END OF it_status.

* Field Symbol
FIELD-SYMBOLS : <hpcp>.      " for HPCC Unique Part

*RANGES
RANGES r_mark FOR ztpp_spec-mark.

*WORK AREA
DATA : wa_flag,              " No data flag.
       wa_matnr_cl           LIKE  mara-matnr,
       wa_matnr_hd           LIKE  mara-matnr,
       wa_model              LIKE  conf_out-atwrt.

DATA : wa_ir_vhp              LIKE  sy-tabix,
       wa_rp_vhp              LIKE  sy-tabix,
       wa_dl_vhp              LIKE  sy-tabix.


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
  PERFORM head_line.

************************************************************************
*             AT USER-COMMAND                                          *
************************************************************************
AT USER-COMMAND.
  PERFORM user_command.

************************************************************************
*             TOP-OF-PAGE DURING LINE-SELECTION                        *
************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  IF sy-ucomm NE 'TRANS'.
    CASE sy-lsind.
      WHEN '1'.
        PERFORM head_line2.
      WHEN '2'.
        PERFORM head_line3.
    ENDCASE.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM data_select.
  clear: wa_flag.
  PERFORM progress_indicator USING 'CREATING HPCC(P) DATA ....'.
  PERFORM ranges_mark.
*  SELECT worder extc intc mark
*     INTO TABLE it_spec
*     FROM ztpp_spec
*     WHERE mark IN r_mark.
  SORT it_spec BY WORDER  .
  DELETE ADJACENT DUPLICATES FROM IT_SPEC COMPARING worder .
  IF it_spec[] IS INITIAL.
    MESSAGE e001 WITH text-004.
  ENDIF.
ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  RANGES_MARK
*&---------------------------------------------------------------------*
FORM ranges_mark.
  DATA: l_data                LIKE sy-datum.
    r_mark-sign   = 'I'.
  CLEAR: l_data, IT_SPEC, IT_SPEC[].
  IF c_ir = 'X'.
    DELETE FROM ZTPPVHP CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
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

  " Processing the Header...
  loop at it_spec.
    it_spec-WORDER = it_spec-WORDER(14).
    clear: it_spec-extc, it_spec-intc  .
    modify it_spec.
  endloop.
ENDFORM.                    " RANGES_MARK

*&---------------------------------------------------------------------*
*&      Form  DATA_JOIN
*&---------------------------------------------------------------------*
FORM data_join.
  LOOP AT it_spec.
    CLEAR : wa_matnr_hd.  " wa_matnr_cl.
    CONCATENATE it_spec-worder it_spec-extc it_spec-intc
                                         INTO wa_matnr_cl.
*   AT NEW worder.
      wa_matnr_hd = it_spec-worder.
      CLEAR : it_wohd, it_wohd[].
      PERFORM ftp_handling_master TABLES it_wohd
                                  USING wa_matnr_hd.

*     CLEAR : it_wocl, it_wocl[].
*     PERFORM ftp_handling_master TABLES it_wocl
*                                 USING wa_matnr_cl.
*   ENDAT.
    IF it_spec-mark = 'I'.
      it_zsppvhp-flg = 'IR'.
    ELSEIF it_spec-mark = 'R'.
      it_zsppvhp-flg = 'RP'.
    ELSEIF it_spec-mark = 'D'.
      it_zsppvhp-flg = 'DL'.
    ENDIF.
    PERFORM move_to_it_zsppvhp.
   IF NOT it_zsppvhp-flg IS INITIAL AND NOT it_zsppvhp-matnr IS INITIAL.
      CASE it_zsppvhp-flg.
        WHEN 'IR'.
          wa_ir_vhp = wa_ir_vhp + 1.
        WHEN 'RP'.
          wa_rp_vhp = wa_rp_vhp + 1.
        WHEN 'DL'.
          wa_dl_vhp = wa_dl_vhp + 1.
      ENDCASE.
      APPEND it_zsppvhp. CLEAR it_zsppvhp.
    ENDIF.
    IF NOT it_zsppvhp1-flg IS INITIAL AND
       NOT it_zsppvhp1-matnr IS INITIAL.
      APPEND it_zsppvhp1.CLEAR it_zsppvhp1.
    ENDIF.
  ENDLOOP.
  SORT it_zsppvhp.
  IF it_zsppvhp[] IS INITIAL.
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
*&      Form  MOVE_TO_IT_ZSPPVHP
*&---------------------------------------------------------------------*
FORM move_to_it_zsppvhp.
  DATA : l_hpcp_fname(40).
  DATA : l_hpcp_search(40).
  DATA : l_no1_nb(1)    TYPE  n,
         l_no2_nb(2)    TYPE  n,
         l_no3_nb(3)    TYPE  n.
  DATA : l_num          TYPE  i,
         l_lines        LIKE  sy-index,
         l_date   LIKE  conf_out-atwrt.

  MOVE : it_zsppvhp-flg TO it_zsppvhp1-flg ,
          wa_matnr_cl   TO it_zsppvhp-matnr,
          wa_matnr_cl   TO it_zsppvhp1-matnr.

*  PERFORM read_wohd_wocl USING 'WOCL' 'P_WO_CREATE_DATE1'
*                         CHANGING l_date.
*  it_zsppvhp-p_wo_create_date = l_date(8).
*  it_zsppvhp-p_wo_create_time = l_date+8(6).
*  MOVE:   it_zsppvhp-p_wo_create_date TO it_zsppvhp1-p_wo_create_date,
*          it_zsppvhp-p_wo_create_time TO it_zsppvhp1-p_wo_create_time.

  DESCRIBE TABLE it_wohd LINES  l_lines.
  IF l_lines NE 0.
    DO 400 TIMES.
      l_no3_nb = sy-index.
      CLEAR : l_hpcp_search, l_hpcp_fname.
      IF sy-index LE 200.
        CONCATENATE 'IT_ZSPPVHP-P'  l_no3_nb INTO l_hpcp_fname.
        ASSIGN (l_hpcp_fname)   TO <hpcp>.
      ELSEIF sy-index GE 201.
        CONCATENATE 'IT_ZSPPVHP1-P'  l_no3_nb INTO l_hpcp_fname.
        ASSIGN (l_hpcp_fname)   TO <hpcp>.
      ENDIF.

      CONCATENATE 'P_WO_HPC_P' l_no3_nb  INTO l_hpcp_search.
      PERFORM read_wohd_wocl USING 'WOHD' l_hpcp_search
                             CHANGING <hpcp>.
    ENDDO.
  ENDIF.
ENDFORM.                    " MOVE_TO_IT_ZSPPVHP

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
*    WHEN 'WOCL'.
*      READ TABLE it_wocl WITH KEY atnam = p_atnam.
*      IF sy-subrc EQ 0.
*        p_atwrt = it_wocl-atwrt.
*      ENDIF.
  ENDCASE.
ENDFORM.                    " READ_WOHD_WOCL

*&---------------------------------------------------------------------*
*&      Form  WRITE_OR_TRANSFER
*&---------------------------------------------------------------------*
FORM write_or_transfer.
  check wa_flag = space .
  IF c_trans = 'X'.
    PERFORM transfer_to_alc.
  ELSE.
    PERFORM data_write.
  ENDIF.
ENDFORM.                    " WRITE_OR_TRANSFER
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_TO_ALC
*&---------------------------------------------------------------------*
FORM transfer_to_alc.
  DATA: l_cn1(6)       TYPE n,
        l_cnt(6)       TYPE n,
        l_msgtxt(100)        .

  PERFORM progress_indicator USING 'TRANSFER FROM SPEC TO ALC'.
*----> EAI TRANSFER
  DESCRIBE TABLE it_zsppvhp1 LINES l_cn1.
  DESCRIBE TABLE it_zsppvhp  LINES l_cnt.

  CALL FUNCTION 'Z_FPP_SET_ZTPPVHP'
    DESTINATION c_dest
    EXPORTING
      ir             = wa_ir_vhp
      rp             = wa_rp_vhp
      dl             = wa_dl_vhp
    TABLES
      i_zsppvhp      = it_zsppvhp
      i_zsppvhp1     = it_zsppvhp1
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc <> 0.
    it_message_p-funcname = 'Z_FPP_SET_ZTPPVHP'.
    it_message_p-msgtxt   = l_msgtxt.
    APPEND it_message_p. CLEAR it_message_p.
  ELSE.
    LOOP AT it_zsppvhp.
      MOVE-CORRESPONDING it_zsppvhp TO it_ztppvhp.
      MOVE sy-uname    TO  it_ztppvhp-zuser.
      MOVE sy-datum    TO  it_ztppvhp-zsdat.
      MOVE sy-uzeit    TO  it_ztppvhp-zstim.
      APPEND it_ztppvhp.
    ENDLOOP.
    IF NOT it_ztppvhp[] IS INITIAL.
      MODIFY ztppvhp FROM TABLE it_ztppvhp.
      IF sy-subrc = 0.
        COMMIT WORK.
        it_message_p-funcname = 'ZTPPVHP'.
        CONCATENATE l_cnt text-500  INTO it_message_p-msgtxt
                                    SEPARATED BY space       .
        APPEND it_message_p. CLEAR it_message_p.
      ELSE.
        ROLLBACK WORK.
        it_message_p-funcname = 'ZTPPVHP'.
        it_message_p-msgtxt   = text-501 .
        APPEND it_message_p. CLEAR it_message_p.
      ENDIF.
    ENDIF.
    LOOP AT it_zsppvhp1.
      MOVE-CORRESPONDING it_zsppvhp1 TO it_ztppvhp1.
      MOVE it_ztppvhp-zuser    TO  it_ztppvhp1-zuser.
      MOVE it_ztppvhp-zsdat    TO  it_ztppvhp1-zsdat.
      MOVE it_ztppvhp-zstim    TO  it_ztppvhp1-zstim.
      APPEND it_ztppvhp1.
    ENDLOOP.
    IF NOT it_ztppvhp1[] IS INITIAL.
      MODIFY ztppvhp FROM TABLE it_ztppvhp.
      IF sy-subrc = 0.
        COMMIT WORK.
        it_message_p-funcname = 'ZTPPVHP1'.
        CONCATENATE l_cnt text-500  INTO it_message_p-msgtxt
                                    SEPARATED BY space       .
        APPEND it_message_p. CLEAR it_message_p.
      ELSE.
        ROLLBACK WORK.
        it_message_p-funcname = 'ZTPPVHP1'.
        it_message_p-msgtxt   = text-501  .
        APPEND it_message_p. CLEAR it_message_p.
      ENDIF.
    ENDIF.

  ENDIF.

  IF sy-ucomm = 'TRANS'.
    MOVE 'TRANS' TO it_status-fcode.
    APPEND it_status. CLEAR it_status.
    SET PF-STATUS 'SUB_LIST' EXCLUDING it_status.
    LOOP AT it_message_p.
      WRITE:/ it_message_p.
    ENDLOOP.
  ELSE.
    EXPORT it_message_p  TO MEMORY ID 'MEMO'.
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
*  PERFORM HEAD_LINE.
  LOOP AT it_zsppvhp.
    zebra = sy-tabix MOD 2.
    IF zebra = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE AT:/1(4)   it_zsppvhp-flg,
              6(18)  it_zsppvhp-matnr,
              25(4)  it_zsppvhp-p001,
              30(4)  it_zsppvhp-p002,
              35(4)  it_zsppvhp-p003,
              40(4)  it_zsppvhp-p004,
              45(4)  it_zsppvhp-p005,
              50(4)  it_zsppvhp-p006,
              55(4)  it_zsppvhp-p007,
              60(4)  it_zsppvhp-p008,
              65(4)  it_zsppvhp-p009,
              70(4)  it_zsppvhp-p010,
              75(4)  it_zsppvhp-p011,
              80(4)  it_zsppvhp-p012,
              85(4)  it_zsppvhp-p013,
              90(4)  it_zsppvhp-p014,
              95(4)  it_zsppvhp-p015,
             100(4)  it_zsppvhp-p016,
             105(4)  it_zsppvhp-p017,
             110(4)  it_zsppvhp-p018,
             115(4)  it_zsppvhp-p019,
             120(4)  it_zsppvhp-p020,
             125(4)  it_zsppvhp-p021,
             130(4)  it_zsppvhp-p022,
             135(4)  it_zsppvhp-p023,
             140(4)  it_zsppvhp-p024,
             145(4)  it_zsppvhp-p025,
             150(4)  it_zsppvhp-p026,
             155(4)  it_zsppvhp-p027,
             160(4)  it_zsppvhp-p028,
             165(4)  it_zsppvhp-p029,
             170(4)  it_zsppvhp-p030,
             175(4)  it_zsppvhp-p031,
             180(4)  it_zsppvhp-p032,
             185(4)  it_zsppvhp-p033,
             190(4)  it_zsppvhp-p034,
             195(4)  it_zsppvhp-p035,
             200(4)  it_zsppvhp-p036,
             205(4)  it_zsppvhp-p037,
             210(4)  it_zsppvhp-p038,
             215(4)  it_zsppvhp-p039,
             220(4)  it_zsppvhp-p040,
             225(4)  it_zsppvhp-p041,
             230(4)  it_zsppvhp-p042,
             235(4)  it_zsppvhp-p043,
             240(4)  it_zsppvhp-p044,
             245(4)  it_zsppvhp-p045,
             250(4)  it_zsppvhp-p046,
             255(4)  it_zsppvhp-p047,
             260(4)  it_zsppvhp-p048,
             265(4)  it_zsppvhp-p049,
             270(4)  it_zsppvhp-p050,
             275(4)  it_zsppvhp-p051,
             280(4)  it_zsppvhp-p052,
             285(4)  it_zsppvhp-p053,
             290(4)  it_zsppvhp-p054,
             295(4)  it_zsppvhp-p055,
             300(4)  it_zsppvhp-p056,
             305(4)  it_zsppvhp-p057,
             310(4)  it_zsppvhp-p058,
             315(4)  it_zsppvhp-p059,
             320(4)  it_zsppvhp-p060,
             325(4)  it_zsppvhp-p061,
             330(4)  it_zsppvhp-p062,
             335(4)  it_zsppvhp-p063,
             340(4)  it_zsppvhp-p064,
             345(4)  it_zsppvhp-p065,
             350(4)  it_zsppvhp-p066,
             355(4)  it_zsppvhp-p067,
             360(4)  it_zsppvhp-p068,
             365(4)  it_zsppvhp-p069,
             370(4)  it_zsppvhp-p070,
             375(4)  it_zsppvhp-p071,
             380(4)  it_zsppvhp-p072,
             385(4)  it_zsppvhp-p073,
             390(4)  it_zsppvhp-p074,
             395(4)  it_zsppvhp-p075,
             400(4)  it_zsppvhp-p076,
             405(4)  it_zsppvhp-p077,
             410(4)  it_zsppvhp-p078,
             415(4)  it_zsppvhp-p079,
             420(4)  it_zsppvhp-p080,
             425(4)  it_zsppvhp-p081,
             430(4)  it_zsppvhp-p082,
             435(4)  it_zsppvhp-p083,
             440(4)  it_zsppvhp-p084,
             445(4)  it_zsppvhp-p085,
             450(4)  it_zsppvhp-p086,
             455(4)  it_zsppvhp-p087,
             460(4)  it_zsppvhp-p088,
             465(4)  it_zsppvhp-p089,
             470(4)  it_zsppvhp-p090,
             475(4)  it_zsppvhp-p091,
             480(4)  it_zsppvhp-p092,
             485(4)  it_zsppvhp-p093,
             490(4)  it_zsppvhp-p094,
             495(4)  it_zsppvhp-p095,
             500(4)  it_zsppvhp-p096,
             505(4)  it_zsppvhp-p097,
             510(4)  it_zsppvhp-p098,
             515(4)  it_zsppvhp-p099,
             520(4)  it_zsppvhp-p100,
             525(4)  it_zsppvhp-p101,
             530(4)  it_zsppvhp-p102,
             535(4)  it_zsppvhp-p103,
             540(4)  it_zsppvhp-p104,
             545(4)  it_zsppvhp-p105,
             550(4)  it_zsppvhp-p106,
             555(4)  it_zsppvhp-p107,
             560(4)  it_zsppvhp-p108,
             565(4)  it_zsppvhp-p109,
             570(4)  it_zsppvhp-p110,
             575(4)  it_zsppvhp-p111,
             580(4)  it_zsppvhp-p112,
             585(4)  it_zsppvhp-p113,
             590(4)  it_zsppvhp-p114,
             595(4)  it_zsppvhp-p115,
             600(4)  it_zsppvhp-p116,
             605(4)  it_zsppvhp-p117,
             610(4)  it_zsppvhp-p118,
             615(4)  it_zsppvhp-p119,
             620(4)  it_zsppvhp-p120,
             625(4)  it_zsppvhp-p121,
             630(4)  it_zsppvhp-p122,
             635(4)  it_zsppvhp-p123,
             640(4)  it_zsppvhp-p124,
             645(4)  it_zsppvhp-p125,
             650(4)  it_zsppvhp-p126,
             655(4)  it_zsppvhp-p127,
             660(4)  it_zsppvhp-p128,
             665(4)  it_zsppvhp-p129,
             670(4)  it_zsppvhp-p130,
             675(4)  it_zsppvhp-p131,
             680(4)  it_zsppvhp-p132,
             685(4)  it_zsppvhp-p133,
             690(4)  it_zsppvhp-p134,
             695(4)  it_zsppvhp-p135,
             700(4)  it_zsppvhp-p136,
             705(4)  it_zsppvhp-p137,
             710(4)  it_zsppvhp-p138,
             715(4)  it_zsppvhp-p139,
             720(4)  it_zsppvhp-p140,
             725(4)  it_zsppvhp-p141,
             730(4)  it_zsppvhp-p142,
             735(4)  it_zsppvhp-p143,
             740(4)  it_zsppvhp-p144,
             745(4)  it_zsppvhp-p145,
             750(4)  it_zsppvhp-p146,
             755(4)  it_zsppvhp-p147,
             760(4)  it_zsppvhp-p148,
             765(4)  it_zsppvhp-p149,
             770(4)  it_zsppvhp-p150,
             775(4)  it_zsppvhp-p151,
             780(4)  it_zsppvhp-p152,
             785(4)  it_zsppvhp-p153,
             790(4)  it_zsppvhp-p154,
             795(4)  it_zsppvhp-p155,
             800(4)  it_zsppvhp-p156,
             805(4)  it_zsppvhp-p157,
             810(4)  it_zsppvhp-p158,
             815(4)  it_zsppvhp-p159,
             820(4)  it_zsppvhp-p160,
             825(4)  it_zsppvhp-p161,
             830(4)  it_zsppvhp-p162,
             835(4)  it_zsppvhp-p163,
             840(4)  it_zsppvhp-p164,
             845(4)  it_zsppvhp-p165,
             850(4)  it_zsppvhp-p166,
             855(4)  it_zsppvhp-p167,
             860(4)  it_zsppvhp-p168,
             865(4)  it_zsppvhp-p169,
             870(4)  it_zsppvhp-p170,
             875(4)  it_zsppvhp-p171,
             880(4)  it_zsppvhp-p172,
             885(4)  it_zsppvhp-p173,
             890(4)  it_zsppvhp-p174,
             895(4)  it_zsppvhp-p175,
             900(4)  it_zsppvhp-p176,
             905(4)  it_zsppvhp-p177,
             910(4)  it_zsppvhp-p178,
             915(4)  it_zsppvhp-p179,
             920(4)  it_zsppvhp-p180,
             925(4)  it_zsppvhp-p181,
             930(4)  it_zsppvhp-p182,
             935(4)  it_zsppvhp-p183,
             940(4)  it_zsppvhp-p184,
             945(4)  it_zsppvhp-p185,
             950(4)  it_zsppvhp-p186,
             955(4)  it_zsppvhp-p187,
             960(4)  it_zsppvhp-p188,
             965(4)  it_zsppvhp-p189,
             970(4)  it_zsppvhp-p190,
             975(4)  it_zsppvhp-p191,
             980(4)  it_zsppvhp-p192,
             985(4)  it_zsppvhp-p193,
             990(4)  it_zsppvhp-p194,
             995(4)  it_zsppvhp-p195,
            1000(4)  it_zsppvhp-p196,
            1005(4)  it_zsppvhp-p197,
            1010(4)  it_zsppvhp-p198,
            1015(4)  it_zsppvhp-p199,
            1020(4)  it_zsppvhp-p200.
*             825(11) IT_ZSPPVHP-P_WO_CREATE_DATE,
*             837(11) IT_ZSPPVHP-P_WO_CREATE_DATE.

  ENDLOOP.
  SET LEFT SCROLL-BOUNDARY COLUMN 24.
ENDFORM.                    " DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  HEAD_LINE
*&---------------------------------------------------------------------*
FORM head_line.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4)  text-006,
               6(18)  text-007,
               25(4)    text-008,
               30(4)    text-009,
               35(4)    text-010,
               40(4)    text-011,
               45(4)    text-012,
               50(4)    text-013,
               55(4)    text-014,
               60(4)    text-015,
               65(4)    text-016,
               70(4)    text-017,
               75(4)    text-018,
               80(4)    text-019,
               85(4)    text-020,
               90(4)    text-021,
               95(4)    text-022,
              100(4)    text-023,
              105(4)    text-024,
              110(4)    text-025,
              115(4)    text-026,
              120(4)    text-027,
              125(4)    text-028,
              130(4)    text-029,
              135(4)    text-030,
              140(4)    text-031,
              145(4)    text-032,
              150(4)    text-033,
              155(4)    text-034,
              160(4)    text-035,
              165(4)    text-036,
              170(4)    text-037,
              175(4)    text-038,
              180(4)    text-039,
              185(4)    text-040,
              190(4)    text-041,
              195(4)    text-042,
              200(4)    text-043,
              205(4)    text-044,
              210(4)    text-045,
              215(4)    text-046,
              220(4)    text-047,
              225(4)    text-048,
              230(4)    text-049,
              235(4)    text-050,
              240(4)    text-051,
              245(4)    text-052,
              250(4)    text-053,
              255(4)    text-054,
              260(4)    text-055,
              265(4)    text-056,
              270(4)    text-057,
              275(4)    text-058,
              280(4)    text-059,
              285(4)    text-060,
              290(4)    text-061,
              295(4)    text-062,
              300(4)    text-063,
              305(4)    text-064,
              310(4)    text-065,
              315(4)    text-066,
              320(4)    text-067,
              325(4)    text-068,
              330(4)    text-069,
              335(4)    text-070,
              340(4)    text-071,
              345(4)    text-072,
              350(4)    text-073,
              355(4)    text-074,
              360(4)    text-075,
              365(4)    text-076,
              370(4)    text-077,
              375(4)    text-078,
              380(4)    text-079,
              385(4)    text-080,
              390(4)    text-081,
              395(4)    text-082,
              400(4)    text-083,
              405(4)    text-084,
              410(4)    text-085,
              415(4)    text-086,
              420(4)    text-087,
              425(4)    text-088,
              430(4)    text-089,
              435(4)    text-090,
              440(4)    text-091,
              445(4)    text-092,
              450(4)    text-093,
              455(4)    text-094,
              460(4)    text-095,
              465(4)    text-096,
              470(4)    text-097,
              475(4)    text-098,
              480(4)    text-099,
              485(4)    text-100,
              490(4)    text-101,
              495(4)    text-102,
              500(4)    text-103,
              505(4)    text-104,
              510(4)    text-105,
              515(4)    text-106,
              520(4)    text-107,
              525(4)    text-108,
              530(4)    text-109,
              535(4)    text-110,
              540(4)    text-111,
              545(4)    text-112,
              550(4)    text-113,
              555(4)    text-114,
              560(4)    text-115,
              565(4)    text-116,
              570(4)    text-117,
              575(4)    text-118,
              580(4)    text-119,
              585(4)    text-120,
              590(4)    text-121,
              595(4)    text-122,
              600(4)    text-123,
              605(4)    text-124,
              610(4)    text-125,
              615(4)    text-126,
              620(4)    text-127,
              625(4)    text-128,
              630(4)    text-129,
              635(4)    text-130,
              640(4)    text-131,
              645(4)    text-132,
              650(4)    text-133,
              655(4)    text-134,
              660(4)    text-135,
              665(4)    text-136,
              670(4)    text-137,
              675(4)    text-138,
              680(4)    text-139,
              685(4)    text-140,
              690(4)    text-141,
              695(4)    text-142,
              700(4)    text-143,
              705(4)    text-144,
              710(4)    text-145,
              715(4)    text-146,
              720(4)    text-147,
              725(4)    text-148,
              730(4)    text-149,
              735(4)    text-150,
              740(4)    text-151,
              745(4)    text-152,
              750(4)    text-153,
              755(4)    text-154,
              760(4)    text-155,
              765(4)    text-156,
              770(4)    text-157,
              775(4)    text-158,
              780(4)    text-159,
              785(4)    text-160,
              790(4)    text-161,
              795(4)    text-162,
              800(4)    text-163,
              805(4)    text-164,
              810(4)    text-165,
              815(4)    text-166,
              820(4)    text-167,
              825(4)    text-168,
              830(4)    text-169,
              835(4)    text-170,
              840(4)    text-171,
              845(4)    text-172,
              850(4)    text-173,
              855(4)    text-174,
              860(4)    text-175,
              865(4)    text-176,
              870(4)    text-177,
              875(4)    text-178,
              880(4)    text-179,
              885(4)    text-180,
              890(4)    text-181,
              895(4)    text-182,
              900(4)    text-183,
              905(4)    text-184,
              910(4)    text-185,
              915(4)    text-186,
              920(4)    text-187,
              925(4)    text-188,
              930(4)    text-189,
              935(4)    text-190,
              940(4)    text-191,
              945(4)    text-192,
              950(4)    text-193,
              955(4)    text-194,
              960(4)    text-195,
              965(4)    text-196,
              970(4)    text-197,
              975(4)    text-198,
              980(4)    text-199,
              985(4)    text-200,
              990(4)    text-201,
              995(4)    text-202,
             1000(4)    text-203,
             1005(4)    text-204,
             1010(4)    text-205,
             1015(4)    text-206,
             1020(4)    text-207.

*            525(11) 'CREATE DATE',
*            537(11) 'CREATE TIME'.


ENDFORM.                    " HEAD_LINE
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.
  CASE sy-ucomm.
    WHEN 'NEXT'.
      IF sy-lsind = '1'.
        PERFORM zsppvhp1_list.
      ELSEIF sy-lsind = '2'.
        PERFORM date_list.
      ENDIF.
    WHEN 'TRANS'.
      PERFORM transfer_to_alc.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  ZSPPVHP1_LIST
*&---------------------------------------------------------------------*
FORM zsppvhp1_list.
  DATA:zebra.
*  PERFORM HEAD_LINE2.
  LOOP AT it_zsppvhp1.
    zebra = sy-tabix MOD 2.
    IF zebra = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE AT:/1(4)   it_zsppvhp1-flg,
              6(18)  it_zsppvhp1-matnr,
              25(4)  it_zsppvhp1-p201,
              30(4)  it_zsppvhp1-p202,
              35(4)  it_zsppvhp1-p203,
              40(4)  it_zsppvhp1-p204,
              45(4)  it_zsppvhp1-p205,
              50(4)  it_zsppvhp1-p206,
              55(4)  it_zsppvhp1-p207,
              60(4)  it_zsppvhp1-p208,
              65(4)  it_zsppvhp1-p209,
              70(4)  it_zsppvhp1-p210,
              75(4)  it_zsppvhp1-p211,
              80(4)  it_zsppvhp1-p212,
              85(4)  it_zsppvhp1-p213,
              90(4)  it_zsppvhp1-p214,
              95(4)  it_zsppvhp1-p215,
             100(4)  it_zsppvhp1-p216,
             105(4)  it_zsppvhp1-p217,
             110(4)  it_zsppvhp1-p218,
             115(4)  it_zsppvhp1-p219,
             120(4)  it_zsppvhp1-p220,
             125(4)  it_zsppvhp1-p221,
             130(4)  it_zsppvhp1-p222,
             135(4)  it_zsppvhp1-p223,
             140(4)  it_zsppvhp1-p224,
             145(4)  it_zsppvhp1-p225,
             150(4)  it_zsppvhp1-p226,
             155(4)  it_zsppvhp1-p227,
             160(4)  it_zsppvhp1-p228,
             165(4)  it_zsppvhp1-p229,
             170(4)  it_zsppvhp1-p230,
             175(4)  it_zsppvhp1-p231,
             180(4)  it_zsppvhp1-p232,
             185(4)  it_zsppvhp1-p233,
             190(4)  it_zsppvhp1-p234,
             195(4)  it_zsppvhp1-p235,
             200(4)  it_zsppvhp1-p236,
             205(4)  it_zsppvhp1-p237,
             210(4)  it_zsppvhp1-p238,
             215(4)  it_zsppvhp1-p239,
             220(4)  it_zsppvhp1-p240,
             225(4)  it_zsppvhp1-p241,
             230(4)  it_zsppvhp1-p242,
             235(4)  it_zsppvhp1-p243,
             240(4)  it_zsppvhp1-p244,
             245(4)  it_zsppvhp1-p245,
             250(4)  it_zsppvhp1-p246,
             255(4)  it_zsppvhp1-p247,
             260(4)  it_zsppvhp1-p248,
             265(4)  it_zsppvhp1-p249,
             270(4)  it_zsppvhp1-p250,
             275(4)  it_zsppvhp1-p251,
             280(4)  it_zsppvhp1-p252,
             285(4)  it_zsppvhp1-p253,
             290(4)  it_zsppvhp1-p254,
             295(4)  it_zsppvhp1-p255,
             300(4)  it_zsppvhp1-p256,
             305(4)  it_zsppvhp1-p257,
             310(4)  it_zsppvhp1-p258,
             315(4)  it_zsppvhp1-p259,
             320(4)  it_zsppvhp1-p260,
             325(4)  it_zsppvhp1-p261,
             330(4)  it_zsppvhp1-p262,
             335(4)  it_zsppvhp1-p263,
             340(4)  it_zsppvhp1-p264,
             345(4)  it_zsppvhp1-p265,
             350(4)  it_zsppvhp1-p266,
             355(4)  it_zsppvhp1-p267,
             360(4)  it_zsppvhp1-p268,
             365(4)  it_zsppvhp1-p269,
             370(4)  it_zsppvhp1-p270,
             375(4)  it_zsppvhp1-p271,
             380(4)  it_zsppvhp1-p272,
             385(4)  it_zsppvhp1-p273,
             390(4)  it_zsppvhp1-p274,
             395(4)  it_zsppvhp1-p275,
             400(4)  it_zsppvhp1-p276,
             405(4)  it_zsppvhp1-p277,
             410(4)  it_zsppvhp1-p278,
             415(4)  it_zsppvhp1-p279,
             420(4)  it_zsppvhp1-p280,
             425(4)  it_zsppvhp1-p281,
             430(4)  it_zsppvhp1-p282,
             435(4)  it_zsppvhp1-p283,
             440(4)  it_zsppvhp1-p284,
             445(4)  it_zsppvhp1-p285,
             450(4)  it_zsppvhp1-p286,
             455(4)  it_zsppvhp1-p287,
             460(4)  it_zsppvhp1-p288,
             465(4)  it_zsppvhp1-p289,
             470(4)  it_zsppvhp1-p290,
             475(4)  it_zsppvhp1-p291,
             480(4)  it_zsppvhp1-p292,
             485(4)  it_zsppvhp1-p293,
             490(4)  it_zsppvhp1-p294,
             495(4)  it_zsppvhp1-p295,
             500(4)  it_zsppvhp1-p296,
             505(4)  it_zsppvhp1-p297,
             510(4)  it_zsppvhp1-p298,
             515(4)  it_zsppvhp1-p299,
             520(4)  it_zsppvhp1-p300,
             525(4)  it_zsppvhp1-p301,
             530(4)  it_zsppvhp1-p302,
             535(4)  it_zsppvhp1-p303,
             540(4)  it_zsppvhp1-p304,
             545(4)  it_zsppvhp1-p305,
             550(4)  it_zsppvhp1-p306,
             555(4)  it_zsppvhp1-p307,
             560(4)  it_zsppvhp1-p308,
             565(4)  it_zsppvhp1-p309,
             570(4)  it_zsppvhp1-p310,
             575(4)  it_zsppvhp1-p311,
             580(4)  it_zsppvhp1-p312,
             585(4)  it_zsppvhp1-p313,
             590(4)  it_zsppvhp1-p314,
             595(4)  it_zsppvhp1-p315,
             600(4)  it_zsppvhp1-p316,
             605(4)  it_zsppvhp1-p317,
             610(4)  it_zsppvhp1-p318,
             615(4)  it_zsppvhp1-p319,
             620(4)  it_zsppvhp1-p320,
             625(4)  it_zsppvhp1-p321,
             630(4)  it_zsppvhp1-p322,
             635(4)  it_zsppvhp1-p323,
             640(4)  it_zsppvhp1-p324,
             645(4)  it_zsppvhp1-p325,
             650(4)  it_zsppvhp1-p326,
             655(4)  it_zsppvhp1-p327,
             660(4)  it_zsppvhp1-p328,
             665(4)  it_zsppvhp1-p329,
             670(4)  it_zsppvhp1-p330,
             675(4)  it_zsppvhp1-p331,
             680(4)  it_zsppvhp1-p332,
             685(4)  it_zsppvhp1-p333,
             690(4)  it_zsppvhp1-p334,
             695(4)  it_zsppvhp1-p335,
             700(4)  it_zsppvhp1-p336,
             705(4)  it_zsppvhp1-p337,
             710(4)  it_zsppvhp1-p338,
             715(4)  it_zsppvhp1-p339,
             720(4)  it_zsppvhp1-p340,
             725(4)  it_zsppvhp1-p341,
             730(4)  it_zsppvhp1-p342,
             735(4)  it_zsppvhp1-p343,
             740(4)  it_zsppvhp1-p344,
             745(4)  it_zsppvhp1-p345,
             750(4)  it_zsppvhp1-p346,
             755(4)  it_zsppvhp1-p347,
             760(4)  it_zsppvhp1-p348,
             765(4)  it_zsppvhp1-p349,
             770(4)  it_zsppvhp1-p350,
             775(4)  it_zsppvhp1-p351,
             780(4)  it_zsppvhp1-p352,
             785(4)  it_zsppvhp1-p353,
             790(4)  it_zsppvhp1-p354,
             795(4)  it_zsppvhp1-p355,
             800(4)  it_zsppvhp1-p356,
             805(4)  it_zsppvhp1-p357,
             810(4)  it_zsppvhp1-p358,
             815(4)  it_zsppvhp1-p359,
             820(4)  it_zsppvhp1-p360,
             825(4)  it_zsppvhp1-p361,
             830(4)  it_zsppvhp1-p362,
             835(4)  it_zsppvhp1-p363,
             840(4)  it_zsppvhp1-p364,
             845(4)  it_zsppvhp1-p365,
             850(4)  it_zsppvhp1-p366,
             855(4)  it_zsppvhp1-p367,
             860(4)  it_zsppvhp1-p368,
             865(4)  it_zsppvhp1-p369,
             870(4)  it_zsppvhp1-p370,
             875(4)  it_zsppvhp1-p371,
             880(4)  it_zsppvhp1-p372,
             885(4)  it_zsppvhp1-p373,
             890(4)  it_zsppvhp1-p374,
             895(4)  it_zsppvhp1-p375,
             900(4)  it_zsppvhp1-p376,
             905(4)  it_zsppvhp1-p377,
             910(4)  it_zsppvhp1-p378,
             915(4)  it_zsppvhp1-p379,
             920(4)  it_zsppvhp1-p380,
             925(4)  it_zsppvhp1-p381,
             930(4)  it_zsppvhp1-p382,
             935(4)  it_zsppvhp1-p383,
             940(4)  it_zsppvhp1-p384,
             945(4)  it_zsppvhp1-p385,
             950(4)  it_zsppvhp1-p386,
             955(4)  it_zsppvhp1-p387,
             960(4)  it_zsppvhp1-p388,
             965(4)  it_zsppvhp1-p389,
             970(4)  it_zsppvhp1-p390,
             975(4)  it_zsppvhp1-p391,
             980(4)  it_zsppvhp1-p392,
             985(4)  it_zsppvhp1-p393,
             990(4)  it_zsppvhp1-p394,
             995(4)  it_zsppvhp1-p395,
            1000(4)  it_zsppvhp1-p396,
            1005(4)  it_zsppvhp1-p397,
            1010(4)  it_zsppvhp1-p398,
            1015(4)  it_zsppvhp1-p399,
            1020(4)  it_zsppvhp1-p400.
  ENDLOOP.
ENDFORM.                    " ZSPPVHP1_LIST
*&---------------------------------------------------------------------*
*&      Form  HEAD_LINE2
*&---------------------------------------------------------------------*
FORM head_line2.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4)  text-208,
              6(18)  text-209,
              25(4)    text-210,
              30(4)    text-211,
              35(4)    text-212,
              40(4)    text-213,
              45(4)    text-214,
              50(4)    text-215,
              55(4)    text-216,
              60(4)    text-217,
              65(4)    text-218,
              70(4)    text-219,
              75(4)    text-220,
              80(4)    text-221,
              85(4)    text-222,
              90(4)    text-223,
              95(4)    text-224,
             100(4)    text-225,
             105(4)    text-226,
             110(4)    text-227,
             115(4)    text-228,
             120(4)    text-229,
             125(4)    text-230,
             130(4)    text-231,
             135(4)    text-232,
             140(4)    text-233,
             145(4)    text-234,
             150(4)    text-235,
             155(4)    text-236,
             160(4)    text-237,
             165(4)    text-238,
             170(4)    text-239,
             175(4)    text-240,
             180(4)    text-241,
             185(4)    text-242,
             190(4)    text-243,
             195(4)    text-244,
             200(4)    text-245,
             205(4)    text-246,
             210(4)    text-247,
             215(4)    text-248,
             220(4)    text-249,
             225(4)    text-250,
             230(4)    text-251,
             235(4)    text-252,
             240(4)    text-253,
             245(4)    text-254,
             250(4)    text-255,
             255(4)    text-256,
             260(4)    text-257,
             265(4)    text-258,
             270(4)    text-259,
             275(4)    text-260,
             280(4)    text-261,
             285(4)    text-262,
             290(4)    text-263,
             295(4)    text-264,
             300(4)    text-265,
             305(4)    text-266,
             310(4)    text-267,
             315(4)    text-268,
             320(4)    text-269,
             325(4)    text-270,
             330(4)    text-271,
             335(4)    text-272,
             340(4)    text-273,
             345(4)    text-274,
             350(4)    text-275,
             355(4)    text-276,
             360(4)    text-277,
             365(4)    text-278,
             370(4)    text-279,
             375(4)    text-280,
             380(4)    text-281,
             385(4)    text-282,
             390(4)    text-283,
             395(4)    text-284,
             400(4)    text-285,
             405(4)    text-286,
             410(4)    text-287,
             415(4)    text-288,
             420(4)    text-289,
             425(4)    text-290,
             430(4)    text-291,
             435(4)    text-292,
             440(4)    text-293,
             445(4)    text-294,
             450(4)    text-295,
             455(4)    text-296,
             460(4)    text-297,
             465(4)    text-298,
             470(4)    text-299,
             475(4)    text-300,
             480(4)    text-301,
             485(4)    text-302,
             490(4)    text-303,
             495(4)    text-304,
             500(4)    text-305,
             505(4)    text-306,
             510(4)    text-307,
             515(4)    text-308,
             520(4)    text-309,
             525(4)    text-310,
             530(4)    text-311,
             535(4)    text-312,
             540(4)    text-313,
             545(4)    text-314,
             550(4)    text-315,
             555(4)    text-316,
             560(4)    text-317,
             565(4)    text-318,
             570(4)    text-319,
             575(4)    text-320,
             580(4)    text-321,
             585(4)    text-322,
             590(4)    text-323,
             595(4)    text-324,
             600(4)    text-325,
             605(4)    text-326,
             610(4)    text-327,
             615(4)    text-328,
             620(4)    text-329,
             625(4)    text-330,
             630(4)    text-331,
             635(4)    text-332,
             640(4)    text-333,
             645(4)    text-334,
             650(4)    text-335,
             655(4)    text-336,
             660(4)    text-337,
             665(4)    text-338,
             670(4)    text-339,
             675(4)    text-340,
             680(4)    text-341,
             685(4)    text-342,
             690(4)    text-343,
             695(4)    text-344,
             700(4)    text-345,
             705(4)    text-346,
             710(4)    text-347,
             715(4)    text-348,
             720(4)    text-349,
             725(4)    text-350,
             730(4)    text-351,
             735(4)    text-352,
             740(4)    text-353,
             745(4)    text-354,
             750(4)    text-355,
             755(4)    text-356,
             760(4)    text-357,
             765(4)    text-358,
             770(4)    text-359,
             775(4)    text-360,
             780(4)    text-361,
             785(4)    text-362,
             790(4)    text-363,
             795(4)    text-364,
             800(4)    text-365,
             805(4)    text-366,
             810(4)    text-367,
             815(4)    text-368,
             820(4)    text-369,
             825(4)    text-370,
             830(4)    text-371,
             835(4)    text-372,
             840(4)    text-373,
             845(4)    text-374,
             850(4)    text-375,
             855(4)    text-376,
             860(4)    text-377,
             865(4)    text-378,
             870(4)    text-379,
             875(4)    text-380,
             880(4)    text-381,
             885(4)    text-382,
             890(4)    text-383,
             895(4)    text-384,
             900(4)    text-385,
             905(4)    text-386,
             910(4)    text-387,
             915(4)    text-388,
             920(4)    text-389,
             925(4)    text-390,
             930(4)    text-391,
             935(4)    text-392,
             940(4)    text-393,
             945(4)    text-394,
             950(4)    text-395,
             955(4)    text-396,
             960(4)    text-397,
             965(4)    text-398,
             970(4)    text-399,
             975(4)    text-400,
             980(4)    text-401,
             985(4)    text-402,
             990(4)    text-403,
             995(4)    text-404,
            1000(4)    text-405,
            1005(4)    text-406,
            1010(4)    text-407,
            1015(4)    text-408,
            1020(4)    text-409.


ENDFORM.                    " HEAD_LINE2
*&---------------------------------------------------------------------*
*&      Form  DATE_LIST
*&---------------------------------------------------------------------*
FORM date_list.
  DATA:zebra.
*  PERFORM HEAD_LINE3.
  LOOP AT it_zsppvhp1.
    zebra = sy-tabix MOD 2.
    IF zebra = 0.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.
    WRITE AT:/1(4)   it_zsppvhp1-flg,
              6(18)  it_zsppvhp1-matnr,
              25(11) it_zsppvhp1-p_wo_create_date,
              37(11) it_zsppvhp1-p_wo_create_time.
  ENDLOOP.
ENDFORM.                    " DATE_LIST
*&---------------------------------------------------------------------*
*&      Form  HEAD_LINE3
*&---------------------------------------------------------------------*
FORM head_line3.
  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE AT:/1(4)    text-410,
            6(18)   text-411,
            25(11)  text-412,
            37(11)  text-413.

ENDFORM.                    " HEAD_LINE3
