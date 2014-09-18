FUNCTION ZIM_KHNP_CUSDEC_HSD_EDI_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFBLNO) LIKE  ZTIDS-ZFBLNO
*"     VALUE(W_ZFCLSEQ) LIKE  ZTIDS-ZFCLSEQ
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXPORTING
*"     REFERENCE(W_EDI_RECORD)
*"  TABLES
*"      REC_HSD STRUCTURE  ZSRECHSD
*"  EXCEPTIONS
*"      CREATE_ERROR
*"----------------------------------------------------------------------
DATA : L_TEXT         LIKE     DD07T-DDTEXT,
       L_TEXT280(280) TYPE     C,
       L_TEXT350(350) TYPE     C,
       L_BOOLEAN      TYPE     C,
       L_TYPE(002)    TYPE     C,
       L_TYPE_TEMP    LIKE     L_TYPE,
       L_CODE(003)    TYPE     C,
       W_LIN_CNT      TYPE     I,
       W_DET_CNT      TYPE     I,
       W_HS_CNT       TYPE     I,
       W_RONO(2)      TYPE     C.

DATA : W_TEXT_AMT06(06) TYPE     C,
       W_TEXT_AMT11(11) TYPE     C,
       W_TEXT_AMT15(15) TYPE     C,
       W_ZERO3(03)      TYPE     C  VALUE '000',
       W_ZERO6(06)      TYPE     C  VALUE '000000',
       W_ZERO8(08)      TYPE     C  VALUE '00000000',
       W_ZERO9(09)      TYPE     C  VALUE '000000000',
       W_ZERO10(10)     TYPE     C  VALUE '0000000000',
       W_ZERO11(11)     TYPE     C  VALUE '00000000000',
       W_ZERO12(12)     TYPE     C  VALUE '000000000000',
       W_ZERO13(13)     TYPE     C  VALUE '0000000000000',
       W_ZERO14(14)     TYPE     C  VALUE '00000000000000',
       W_ZERO15(15)     TYPE     C  VALUE '000000000000000'.

*DATA : BEGIN OF REC_HSD,
*       REC_001(015)   TYPE     C,          " 신고번호.
*       REC_002(003)   TYPE     C,          " 란번호.
*       REC_003(003)   TYPE     C,          " 항번호.
*       REC_004(012)   TYPE     C,          " PART NO.
*       REC_005(050)   TYPE     C,          " 품명.
*       REC_006(011)   TYPE     C,          " 수량.
*       REC_007(002)   TYPE     C,          " 수량단위.
*       REC_008(011)   TYPE     C,          " 단가.
*       REC_009(020)   TYPE     C,          " KEY FIELD.
*       REC_010(011)   TYPE     C,          " 관세.
*       REC_011(011)   TYPE     C,          " 특소세.
*       REC_012(011)   TYPE     C,          " 교육세.
*       REC_013(011)   TYPE     C,          " 농특세.
*       REC_014(011)   TYPE     C,          " 주세.
*       REC_015(011)   TYPE     C,          " 부가세.
*       REC_016(011)   TYPE     C,          " 예정운송.
*       REC_017(011)   TYPE     C,          " 예정원가.
*       REC_018(015)   TYPE     C,          " 금액.
*       REC_019(011)   TYPE     C,          " 운임.
*       REC_020(011)   TYPE     C,          " 보험료.
*       REC_021(013)   TYPE     C,          " 과세원화.
*       REC_022(013)   TYPE     C,          " 과세달러.
*       REC_023(010)   TYPE     C,          " 세번부호.
*       REC_024(100)   TYPE     C,          " SPEC.
*       REC_025(030)   TYPE     C,          " 규격1.
*       REC_026(030)   TYPE     C,          " 규격2.
*       REC_027(030)   TYPE     C,          " 규격3.
*       REC_028(025)   TYPE     C,          " 성분1.
*       REC_029(025)   TYPE     C.          " 성분2.
**       CR_LF          TYPE     X        VALUE '0A'.
*DATA : END   OF REC_HSD.

*> CUSTOM CLEARANCE DATA GET
   CALL FUNCTION 'ZIM_GET_IDR_DOCUMENT'
        EXPORTING
              ZFBLNO             =       W_ZFBLNO
              ZFCLSEQ            =       W_ZFCLSEQ
        IMPORTING
              W_ZTIDR            =       ZTIDR
        TABLES
              IT_ZSIDRHS         =       IT_ZSIDRHS
              IT_ZSIDRHS_ORG     =       IT_ZSIDRHS_ORG
              IT_ZSIDRHSD        =       IT_ZSIDRHSD
              IT_ZSIDRHSD_ORG    =       IT_ZSIDRHSD_ORG
              IT_ZSIDRHSL        =       IT_ZSIDRHSL
              IT_ZSIDRHSL_ORG    =       IT_ZSIDRHSL_ORG
        EXCEPTIONS
              NOT_FOUND     =       4
              NOT_INPUT     =       8.

   CASE SY-SUBRC.
      WHEN 4.
         MESSAGE E018 WITH W_ZFREQNO RAISING  CREATE_ERROR.
      WHEN 8.
         MESSAGE E019 RAISING  CREATE_ERROR.
   ENDCASE.

*>> BL 자료 GET!
   CLEAR ZTBL.
   SELECT SINGLE * FROM ZTBL      WHERE ZFBLNO EQ W_ZFBLNO.

*-----------------------------------------------------------------------
* FLAT-FILE RECORD CREATE
*-----------------------------------------------------------------------
   LOOP  AT  IT_ZSIDRHSD.
      CLEAR : REC_HSD, W_EDI_RECORD.

      CONCATENATE  ZTIDR-ZFBLNO  ZTIDR-ZFCLSEQ INTO  REC_HSD-REC_009.

      WRITE : IT_ZSIDRHSD-ZFCONO  TO REC_HSD-REC_002
                                     NO-ZERO RIGHT-JUSTIFIED,
              IT_ZSIDRHSD-ZFRONO  TO W_RONO NO-ZERO RIGHT-JUSTIFIED.
      OVERLAY : REC_HSD-REC_002  WITH '00',
                W_RONO           WITH '00'.

      CONCATENATE :  W_RONO '0'          INTO REC_HSD-REC_003,
                     W_RONO '          ' INTO REC_HSD-REC_004.

      MOVE : SPACE                TO  REC_HSD-REC_005,   " 품명.
             'EA'                 TO  REC_HSD-REC_007,   " 수량단위.
             '00000000000'        TO  REC_HSD-REC_010,   " 관세.
             '00000000000'        TO  REC_HSD-REC_011,   " 특소세.
             '00000000000'        TO  REC_HSD-REC_012,   " 교육세.
             '00000000000'        TO  REC_HSD-REC_013,   " 농특세.
             '00000000000'        TO  REC_HSD-REC_014,   " 주세.
             '00000000000'        TO  REC_HSD-REC_015,   " 부가세.
             '00000000000'        TO  REC_HSD-REC_016,   " 예정운송.
             '00000000000'        TO  REC_HSD-REC_017,   " 예정원가.
             '00000000000'        TO  REC_HSD-REC_019,   " 운임.
             '00000000000'        TO  REC_HSD-REC_020,   " 보험료.
             '0000000000000'      TO  REC_HSD-REC_021,   " 과세원화.
             '0000000000000'      TO  REC_HSD-REC_022,   " 과세달러.
             IT_ZSIDRHSD-STAWN    TO  REC_HSD-REC_023,   " 세번부호.
             SPACE                TO  REC_HSD-REC_024,   " SPEC.
             IT_ZSIDRHSD-ZFGDDS1  TO  REC_HSD-REC_025,   " 규격1.
             IT_ZSIDRHSD-ZFGDDS2  TO  REC_HSD-REC_026,   " 규격2.
             IT_ZSIDRHSD-ZFGDDS3  TO  REC_HSD-REC_027,   " 규격3.
             IT_ZSIDRHSD-ZFGDIN1  TO  REC_HSD-REC_028,   " 성분1.
             IT_ZSIDRHSD-ZFGDIN2  TO  REC_HSD-REC_029.   " 성분2.

      " 수량.
      IF IT_ZSIDRHSD-ZFQNT IS INITIAL.
         MOVE  '00000000.00'  TO  REC_HSD-REC_006.
      ELSE.
         WRITE    IT_ZSIDRHSD-ZFQNT  TO  W_TEXT_AMT11 DECIMALS 2.
         PERFORM  P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT11.
         WRITE    W_TEXT_AMT11  TO  REC_HSD-REC_006 RIGHT-JUSTIFIED.
         OVERLAY  REC_HSD-REC_006  WITH  W_ZERO11.
      ENDIF.

      " 단가.
      IF IT_ZSIDRHSD-NETPR IS INITIAL.
         MOVE  '00000000.00'  TO  REC_HSD-REC_008.
      ELSE.
         WRITE    IT_ZSIDRHSD-NETPR  TO  W_TEXT_AMT11 DECIMALS 2.
         PERFORM  P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT11.
         WRITE    W_TEXT_AMT11 TO REC_HSD-REC_008 RIGHT-JUSTIFIED.
         OVERLAY  REC_HSD-REC_008  WITH  W_ZERO11.
      ENDIF.

      " 금액.
      IF IT_ZSIDRHSD-ZFAMT IS INITIAL.
         MOVE  '000000000000.00'  TO  REC_HSD-REC_018.
      ELSE.
         WRITE    IT_ZSIDRHSD-ZFAMT  TO  W_TEXT_AMT15 DECIMALS 2.
         PERFORM  P2000_WRITE_NO_MASK  CHANGING  W_TEXT_AMT15.
         WRITE    W_TEXT_AMT15 TO REC_HSD-REC_018 RIGHT-JUSTIFIED.
         OVERLAY  REC_HSD-REC_018  WITH  W_ZERO15.
      ENDIF.
      APPEND  REC_HSD.
*      BUFFER_POINTER    =   STRLEN( W_EDI_RECORD ).
*      PARA_LENG         =   STRLEN( REC_HSD ).
*      MOVE  REC_HSD    TO   W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).

   ENDLOOP.

ENDFUNCTION.
