************************************************************************
* Program Name      : ZISD15U_VEHICLE_ORDER_CONF
* Author            : jun ho choi
* Creation Date     : 2003.11.05.
* Specifications By : jun ho choi
* Pattern           : 5-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Vehicle Order confirmation.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 12/17/2004 CHRIS        UD1K912558   EXCLUDING TEST CARS FROM FILE
*
*
************************************************************************
REPORT ZISD15U_VEHICLE_ORDER_CONF NO STANDARD PAGE HEADING
                                  MESSAGE-ID ZMSD.


*
TABLES : ZTPP_WOSUM,
         CABN,
         AUSP.


*
DATA : BEGIN OF IT_WOSUM OCCURS 0.
        INCLUDE STRUCTURE ZTPP_WOSUM.
DATA : END OF IT_WOSUM.

DATA : BEGIN OF IT_R1 OCCURS 0,
       RECORD(120),
       END OF IT_R1.

DATA : BEGIN OF IT_R2_HMA OCCURS 0,
       RECORD(120),
       END OF IT_R2_HMA.

DATA : BEGIN OF IT_R2_HAC OCCURS 0,
       RECORD(120),
       END OF IT_R2_HAC.

DATA : BEGIN OF IT_R3 OCCURS 0,
       RECORD(120),
       END OF IT_R3.

DATA : W_CNT(5) TYPE N,
       W_QTY LIKE IT_WOSUM-INITQTY,
       " W_DSN_B(20) VALUE '/usr/sap/EDI_SAP/',
       W_DSN(90),
       W_COLOR_SER(2),
       W_N_8(5) TYPE N,
       W_MOD_HAC LIKE IT_WOSUM-MODQTY,
       W_MOD_HMA LIKE IT_WOSUM-MODQTY.

DATA : VARIANT LIKE INDX-SRTFD VALUE 'ZISD15_01'.

*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_DATE LIKE SY-DATUM DEFAULT SY-DATUM,
             p_hacfn like rlgrap-filename default '/usr/sap/EDI_SAP/',
             p_hmafn like rlgrap-filename default '/usr/sap/EDI_SAP/',
             p_hmmafn like rlgrap-filename.
SELECTION-SCREEN END OF BLOCK B1.


*
START-OF-SELECTION.
  MESSAGE i000 WITH 'DO NOT USE THIS PROGRAM'.
  LEAVE PROGRAM.

  PERFORM GET_DATA.
  PERFORM MODIFY_DATA.
  PERFORM DOWNLOAD_DATA.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  IF SY-BATCH EQ 'X'.
    IMPORT P_DATE FROM DATABASE INDX(ZZ) ID VARIANT.
  ENDIF.

  REFRESH : IT_WOSUM.
  CLEAR   : IT_WOSUM.

  SELECT *
         INTO TABLE IT_WOSUM
         FROM ZTPP_WOSUM
        WHERE WOMODDATE GT P_DATE
        AND   SALES     NE SPACE.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA.
  DESCRIBE TABLE IT_WOSUM LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    STOP.
  ENDIF.

  REFRESH : IT_R1, IT_R2_HMA, IT_R2_HAC, IT_R3.
  CLEAR   : IT_R1, IT_R2_HMA, IT_R2_HAC, IT_R3.

  CLEAR : W_MOD_HAC, W_MOD_HMA.

  LOOP AT IT_WOSUM.
* REQUESTED BY CATHERINE S. CHANGED BY CHHRIS

*    CASE IT_WOSUM-NATION.
*      WHEN 'B06'. "HAC
     IF IT_WOSUM-NATION = 'B06' AND
       ( IT_WOSUM-DEALER = 'AB' OR
         IT_WOSUM-DEALER = 'AA' ).
        PERFORM MAKE_HAC.
     ENDIF.
*      WHEN 'B28'. "HMA
     IF IT_WOSUM-NATION = 'B28' AND
       ( IT_WOSUM-DEALER = 'AB' OR
         IT_WOSUM-DEALER = 'AA' ).

        PERFORM MAKE_HMA.

     ENDIF.
*    ENDCASE.
* END OF CHANGE ON 12/17/2004
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_HAC
*&---------------------------------------------------------------------*
FORM MAKE_HAC.
  IT_R2_HAC-RECORD+0(03)  = 'O1D'.                        "1
  IT_R2_HAC-RECORD+3(02)  = '20'.                         "2
  IT_R2_HAC-RECORD+5(02)  = IT_WOSUM-WO_SER+1(2).         "2
  IT_R2_HAC-RECORD+7(02)  = IT_WOSUM-WO_SER+3(2).         "3
  IT_R2_HAC-RECORD+9(03)  = IT_WOSUM-NATION.              "4
  IT_R2_HAC-RECORD+12(02) = IT_WOSUM-DEALER.              "4
  IT_R2_HAC-RECORD+14(01) = IT_WOSUM-WO_SER+5(1).         "5
  IT_R2_HAC-RECORD+15(01) = IT_WOSUM-WO_SER+2(1).         "6
  IT_R2_HAC-RECORD+16(02) = IT_WOSUM-WO_SER+3(2).         "6
  IT_R2_HAC-RECORD+18(03) = IT_WOSUM-WO_SER+6(3).         "6

  PERFORM GET_COLOR_SER USING IT_WOSUM-WO_SER
                              IT_WOSUM-NATION
                              IT_WOSUM-DEALER
                              IT_WOSUM-EXTC
                              IT_WOSUM-INTC.
  IT_R2_HAC-RECORD+21(02) = W_COLOR_SER.                  "6
  IT_R2_HAC-RECORD+23(10) = IT_WOSUM-SALES.               "7
  IT_R2_HAC-RECORD+33(10) = IT_WOSUM-WO_SER.              "
  IT_R2_HAC-RECORD+43(18) = IT_WOSUM-FSC.                 "8
  IT_R2_HAC-RECORD+61(03) = IT_WOSUM-EXTC.                "9
  IT_R2_HAC-RECORD+64(03) = IT_WOSUM-INTC.                "10
  IT_R2_HAC-RECORD+67(02) = ''.                           "11
  W_N_8 = IT_WOSUM-INITQTY.                               "12
  IT_R2_HAC-RECORD+69(05) = W_N_8.                        "12
*  IF IT_WOSUM-INITQTY > 0.                                "12
*    IT_R2_HAC-RECORD+69(01) = ''.                         "12
*  ELSE.                                                   "12
*    IT_R2_HAC-RECORD+69(01) = '-'.                        "12
*  ENDIF.                                                  "12
  W_N_8 = IT_WOSUM-MODQTY.                                "13
  IT_R2_HAC-RECORD+74(05) = W_N_8.                        "13
*  IF IT_WOSUM-MODQTY > 0.                                 "13
*    IT_R2_HAC-RECORD+75(01) = ''.                         "13
*  ELSE.                                                   "13
*    IT_R2_HAC-RECORD+75(01) = '-'.                        "13
*  ENDIF.                                                  "13
  IT_R2_HAC-RECORD+79(05) = '00000'.                      "14
  IT_R2_HAC-RECORD+84(05) = '00000'.                      "15
  IT_R2_HAC-RECORD+89(05) = '00000'.                      "16
  IT_R2_HAC-RECORD+94(17) = ''.                           "17

  W_MOD_HAC = W_MOD_HAC + IT_WOSUM-MODQTY.
  APPEND IT_R2_HAC.
ENDFORM.                    " MAKE_HAC
*&---------------------------------------------------------------------*
*&      Form  MAKE_HMA
*&---------------------------------------------------------------------*
FORM MAKE_HMA.
* REC TYPE
  IT_R2_HMA-RECORD+0(03)  = 'O1D'.                        "1
* Year
  IT_R2_HMA-RECORD+3(02)  = '20'.                         "2
  IT_R2_HMA-RECORD+5(02)  = IT_WOSUM-WO_SER+1(2).         "2
* Month
  IT_R2_HMA-RECORD+7(02)  = IT_WOSUM-WO_SER+3(2).         "3
* Distributor Code
  IT_R2_HMA-RECORD+9(03)  = IT_WOSUM-NATION.              "4
  IT_R2_HMA-RECORD+12(02) = IT_WOSUM-DEALER.              "4
* Region/Port ID
  IT_R2_HMA-RECORD+14(01) = IT_WOSUM-WO_SER+5(1).         "5
* HMA Order Number
  IT_R2_HMA-RECORD+15(01) = IT_WOSUM-WO_SER+2(1).         "6
  IT_R2_HMA-RECORD+16(02) = IT_WOSUM-WO_SER+3(2).         "6
  IT_R2_HMA-RECORD+18(03) = IT_WOSUM-WO_SER+6(3).         "6
  PERFORM GET_COLOR_SER USING IT_WOSUM-WO_SER
                              IT_WOSUM-NATION
                              IT_WOSUM-DEALER
                              IT_WOSUM-EXTC
                              IT_WOSUM-INTC.
  IT_R2_HMA-RECORD+21(02) = W_COLOR_SER.                  "6
* HMMA S/O Number
  IT_R2_HMA-RECORD+23(10) = IT_WOSUM-SALES.               "7
* W/O Serial
  IT_R2_HMA-RECORD+33(10) = IT_WOSUM-WO_SER.              "
* FSC
  IT_R2_HMA-RECORD+43(18) = IT_WOSUM-FSC.                 "8
* Ext.C
  IT_R2_HMA-RECORD+61(03) = IT_WOSUM-EXTC.                "9
* Int.C
  IT_R2_HMA-RECORD+64(03) = IT_WOSUM-INTC.                "10
* Priority
  IT_R2_HMA-RECORD+67(02) = ''.                           "11
* Original Order Qty
  W_N_8 = IT_WOSUM-INITQTY.                               "12
  IT_R2_HMA-RECORD+69(05) = W_N_8.                        "12
*  IF IT_WOSUM-INITQTY > 0.                                "12
*    IT_R2_HMA-RECORD+69(01) = ''.                         "12
*  ELSE.                                                   "12
*    IT_R2_HMA-RECORD+69(01) = '-'.                        "12
*  ENDIF.                                                  "12
* Modified Qty
  W_N_8 = IT_WOSUM-MODQTY.                                "13
  IT_R2_HMA-RECORD+74(05) = W_N_8.                        "13
*  IF IT_WOSUM-MODQTY > 0.                                 "13
*    IT_R2_HMA-RECORD+75(01) = ''.                         "13
*  ELSE.                                                   "13
*    IT_R2_HMA-RECORD+75(01) = '-'.                        "13
*  ENDIF.                                                  "13
* Delivered Qty
  IT_R2_HMA-RECORD+79(05) = '00000'.                      "14
* Scheduled Qty
  IT_R2_HMA-RECORD+84(05) = '00000'.                      "15
* Cancelled Qty
  IT_R2_HMA-RECORD+89(05) = '00000'.                      "16
* Filler
  IT_R2_HMA-RECORD+94(17) = ''.                           "17

  W_MOD_HMA = W_MOD_HMA + IT_WOSUM-MODQTY.
  APPEND IT_R2_HMA.

ENDFORM.                    " MAKE_HMA
*&---------------------------------------------------------------------*
*&      Form  GET_COLOR_SER
*&---------------------------------------------------------------------*
FORM GET_COLOR_SER USING K1 K2 K3 K4 K5.
  DATA : W_OBJEK LIKE AUSP-OBJEK.

  CONCATENATE K1 K2 K3 K4 K5 INTO W_OBJEK.

  CLEAR W_COLOR_SER.

  SELECT SINGLE *
         FROM CABN
        WHERE ATNAM = 'P_COLOR_SER'.

  SELECT SINGLE *
         FROM AUSP
        WHERE OBJEK = W_OBJEK
        AND   ATINN = CABN-ATINN.
  IF SY-SUBRC = 0.
    W_COLOR_SER = AUSP-ATWRT.
  ENDIF.
ENDFORM.                    " GET_COLOR_SER
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
FORM DOWNLOAD_DATA.
  DESCRIBE TABLE IT_R2_HAC LINES W_CNT.
  IF W_CNT <> 0.
    PERFORM DOWNLOAD_HAC USING W_CNT.
  ENDIF.

  DESCRIBE TABLE IT_R2_HMA LINES W_CNT.
  IF W_CNT <> 0.
    PERFORM DOWNLOAD_HMA USING W_CNT.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_HAC
*&---------------------------------------------------------------------*
FORM DOWNLOAD_HAC USING W_CNT.
**  CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**               'HAC' SY-DATUM+2(6)
**               'C.txt'
**               INTO W_DSN.
  CONCATENATE  p_hacfn
               'HMMAOC' SY-DATUM+2(6)
               'C.txt'
               INTO W_DSN.

  OPEN DATASET W_DSN IN TEXT MODE FOR OUTPUT.
  PERFORM MAKE_R1.

  LOOP AT IT_R2_HAC.

    TRANSFER IT_R2_HAC TO W_DSN.
  ENDLOOP.
  PERFORM MAKE_R3 USING W_CNT 'HAC'.

  CLOSE DATASET W_DSN.

  IF SY-SUBRC = 0.
    MESSAGE I000 WITH TEXT-M02 '(HAC)'.
  ELSE.
    MESSAGE I000 WITH TEXT-M03 '(HAC)'.
  ENDIF.
ENDFORM.                    " DOWNLOAD_HAC
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_HMA
*&---------------------------------------------------------------------*
FORM DOWNLOAD_HMA USING W_CNT.
**  CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**               'HM_' SY-DATUM+2(6)
**               'C.txt'
**               INTO W_DSN.
  CONCATENATE  '/usr/sap/EDI_SAP/'
               'HM_' SY-DATUM+2(6)
               'C.txt'
               INTO W_DSN.


  OPEN DATASET W_DSN IN TEXT MODE FOR OUTPUT.
  PERFORM MAKE_R1.
  LOOP AT IT_R2_HMA.
    TRANSFER IT_R2_HMA TO W_DSN.
  ENDLOOP.
  PERFORM MAKE_R3 USING W_CNT 'HMA'.

  CLOSE DATASET W_DSN.

  IF SY-SUBRC = 0.
    MESSAGE I000 WITH TEXT-M02 '(HMA)'.
  ELSE.
    MESSAGE I000 WITH TEXT-M03 '(HMA)'.
  ENDIF.
ENDFORM.                    " DOWNLOAD_HMA
*&---------------------------------------------------------------------*
*&      Form  MAKE_R1
*&---------------------------------------------------------------------*
FORM MAKE_R1.
  REFRESH IT_R1.

  IT_R1-RECORD+0(3)   = 'O1H'.
  IT_R1-RECORD+3(8)   = SY-DATUM.
  IT_R1-RECORD+11(6)  = SY-UZEIT.
  IT_R1-RECORD+17(94) = ''.

  APPEND IT_R1.
*  OPEN DATASET W_DSN IN TEXT MODE FOR OUTPUT. "CHECK DUPL.
  LOOP AT IT_R1.
    TRANSFER IT_R1 TO W_DSN.
  ENDLOOP.
ENDFORM.                                                    " MAKE_R1
*&---------------------------------------------------------------------*
*&      Form  MAKE_R3
*&---------------------------------------------------------------------*
FORM MAKE_R3 USING W_CNT GUBUN.
  REFRESH IT_R3.

  IT_R3-RECORD+0(3)   = 'O1T'.
  IT_R3-RECORD+3(5)   = W_CNT.
  CASE GUBUN.
    WHEN 'HAC'.
      IT_R3-RECORD+8(5) = W_MOD_HAC.
      IT_R3-RECORD+13(98) = ''.
    WHEN 'HMA'.
      IT_R3-RECORD+8(5) = W_MOD_HMA.
      IT_R3-RECORD+13(98) = ''.
  ENDCASE.
* IT_R3-RECORD+12(68) = ''.

  APPEND IT_R3.

  LOOP AT IT_R3.
*    OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
    TRANSFER IT_R3 TO W_DSN.
  ENDLOOP.
ENDFORM.                                                    " MAKE_R3
