************************************************************************
* Program Name      : ZSSD10R_HAC_MANIFEST
* Author            : jun ho choi
* Creation Date     : 2004.01.24.
* Specifications By : jun ho choi
* Pattern           : 5-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : HAC MANIFEST.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 01/24/2005  Chris       UD1K913940  chang the layout and add a field
* 07/12/2005  chris       UD1K916796  distribution channel = '10'
* 11/02/2006  Manju       UD1K922885  XM radio ID for Canada Manifest
************************************************************************
REPORT ZSSD08R_HMA_MANIFEST NO STANDARD PAGE HEADING
                            LINE-SIZE 200
                            MESSAGE-ID ZMSD.


*
TABLES : AUSP, CABN, VBFA, VBRK, VBRP.


*
DATA : BEGIN OF IT_AUSP OCCURS 0.
        INCLUDE STRUCTURE AUSP.
DATA : END OF IT_AUSP.

DATA : BEGIN OF IT_VBRP OCCURS 0,
         VBELN LIKE VBRP-VBELN,  "Billing document
         FKDAT LIKE VBRK-FKDAT,  "Billing date
         VGBEL LIKE VBRP-VGBEL,  "Vehicle No.
        "Net value of the billing item in document currency
         NETWR LIKE VBRP-NETWR,
         ZTERM LIKE VBRK-ZTERM,
         VBTYP LIKE VBRK-FKTYP,  " UD1K913940
         FKSTO LIKE VBRK-FKSTO,  " UD1K913940
         SFAKN LIKE VBRK-SFAKN,  " UD1K913940
         SHKZG LIKE VBRP-SHKZG,  " UD1K913940
       END OF IT_VBRP.

DATA : BEGIN OF IT_VBRK_CAN OCCURS 0,
         VBELN LIKE VBRP-VBELN,  "Billing document
         FKDAT LIKE VBRK-FKDAT,  "Billing date
         VBTYP LIKE VBRK-FKTYP,  " UD1K913940
         FKSTO LIKE VBRK-FKSTO,  " UD1K913940
         SFAKN LIKE VBRK-SFAKN,  " UD1K913940
       END OF IT_VBRK_CAN.

RANGES R_ATINN FOR AUSP-ATINN OCCURS 0.

DATA : BEGIN OF IT_CABN OCCURS 0,
       ATINN LIKE CABN-ATINN,
       ATNAM LIKE CABN-ATNAM,
       END OF IT_CABN.

DATA : BEGIN OF IT_DOWN OCCURS 0,
*       record(165),  "UD1K922885
        RECORD(187),                                        "UD1K922885
       END OF IT_DOWN.

DATA : BEGIN OF IT_R1 OCCURS 0,
       F1(02),
       F2(06),  "YYYYMM
       F3(01),
       F4(08),
       F5(02),
       F6(02),
       F7(04),
       F8(01),
       F9(01),
       F10(10),   " change from length 14
       F11(10),
       END OF IT_R1.

DATA : BEGIN OF IT_R2 OCCURS 0,
       F1(02),
       F2(17),  " length change from 18
       F3(15),
       F4(06),
       F5(08),
       F6(01),
       F7(18),
       F8(03), "external color
       F9(03), "internal color
       FOB(08),
       RDSER(20),
       END OF IT_R2.

DATA : BEGIN OF IT_R3 OCCURS 0,
       F1(02),
       F2(09),
       F3(08),
       F4(30),    "XM Radio ID /UD1K922885
       END OF IT_R3.

DATA : OK_CODE(4),
       SAVE_OK_CODE(4).

DATA : W_BILL(9) TYPE N,  "The Total of Billing Document
       W_CNT(9)  TYPE N,  "The Total of Vehicle
       W_TEMP(9) TYPE N,
       W_NETPR  LIKE VBRP-NETWR ,      "Net price
       W_AMOUNT LIKE VBRP-NETWR ,      "Total amount
       W_AMOUNTC(13),
       W_DATE LIKE VBRK-FKDAT,
       W_TERM LIKE VBRK-ZTERM,
       W_ATINN LIKE CABN-ATINN,
       W_ATNAM LIKE CABN-ATNAM,
       W_N_8(8) TYPE N,
       W_OBJEK LIKE AUSP-OBJEK,
       " W_DSN_B(20) VALUE '/usr/sap/EDI_SAP/',
       W_DSN(90).

CONSTANTS: C_HAC LIKE KNA1-KUNNR VALUE 'C001'.
*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_SDATE FOR SY-DATUM NO-EXTENSION,  " OBLIGATORY
                 S_VBELN FOR VBRK-VBELN.
*PARAMETERS :     P_BL AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.


*
START-OF-SELECTION.
  SET PF-STATUS 'SSD08R'.
  PERFORM INIT_CABN.
  PERFORM GET_DATA.
  PERFORM MODIFY_DATA.
  PERFORM DISPLAY_DATA.
*
END-OF-SELECTION.

*
AT USER-COMMAND.
  PERFORM USER_COMMAND.


*&---------------------------------------------------------------------*
*&      Form  INIT_CABN
*&---------------------------------------------------------------------*
FORM INIT_CABN.
  REFRESH : IT_CABN, R_ATINN.
  CLEAR   : IT_CABN, R_ATINN.

  R_ATINN-SIGN = 'I'.
  R_ATINN-OPTION = 'EQ'.

  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_WORK_ORDER'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_REGION_PORT'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_ORDER_ZONE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_FLEET'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_MANUAL_ORDER'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_SALES_ORDER'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_VIN'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_ENGINE_NO'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_KEY_NO'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP18_SHOP_DATE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_MODEL_YEAR'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_DESTINATION_CODE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
**>>> Added By Tonkey 06/29/2004.
         WHERE ATNAM = 'P_MI'.
**<<< Added By Tonkey.
**>>> Inactivated By Tonkey 06/29/2004.
*        where atnam = 'P_MODEL_INDEX'.
**<<< Inactivated By Tonkey.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_OCN'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_COLOR_SER'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.

* changed by chris: adding p_ext_color p_int_color
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_EXT_COLOR'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_INT_COLOR'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.


* end of change on 02/15/2005
* Begin of changes - UD1K922885
  SELECT SINGLE ATINN ATNAM
           INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
           FROM CABN
          WHERE ATNAM = 'P_AIRBAG_NO10'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
* End of changes - UD1K922885

ENDFORM.                    " INIT_CABN
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  IF S_VBELN[] IS INITIAL.
    SELECT K~VBELN K~FKDAT P~VGBEL P~NETWR
           K~VBTYP K~FKSTO K~SFAKN P~SHKZG K~ZTERM
      INTO CORRESPONDING FIELDS OF TABLE IT_VBRP
      FROM VBRK AS K INNER JOIN VBRP AS P
        ON K~VBELN EQ P~VBELN
     WHERE K~KUNRG =  C_HAC
       AND K~VTWEG EQ '10'   "added by chris  UD1K916796
      AND K~FKDAT IN S_SDATE AND K~VBTYP = 'M'.  "UD1K943969 HASEEB, BEN
  ELSE.
** Changed by Furong ON 08/22/08
    SELECT K~VBELN K~FKDAT P~VGBEL P~NETWR
           K~VBTYP K~FKSTO K~SFAKN P~SHKZG K~ZTERM
      INTO CORRESPONDING FIELDS OF TABLE IT_VBRP
      FROM VBRK AS K INNER JOIN VBRP AS P
        ON K~VBELN EQ P~VBELN
     WHERE K~VBELN IN S_VBELN
       AND K~KUNRG =  C_HAC
       AND K~VTWEG EQ '10'   "added by chris  UD1K916796
*      AND K~FKDAT IN S_SDATE
      AND K~VBTYP = 'M'.  "UD1K943969 HASEEB, BEN
**End of change ON 08/22/08
  ENDIF.

** Changed by Furong on 08/20/08  for cancellation
  SELECT VBELN FKDAT
         VBTYP FKSTO SFAKN
         INTO CORRESPONDING FIELDS OF TABLE IT_VBRK_CAN
         FROM VBRK
   WHERE KUNRG =  C_HAC
     AND VTWEG EQ '10'   "added by chris  UD1K916796
     AND FKDAT IN S_SDATE AND VBTYP = 'N'.  "UD1K943969 HASEEB, BEN

** End of change

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
FORM GET_ATINN USING P_ATNAM.
  READ TABLE IT_CABN WITH KEY ATNAM = P_ATNAM.
  IF SY-SUBRC = 0.
    W_ATINN = IT_CABN-ATINN.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  GET_ATNAM
*&---------------------------------------------------------------------*
FORM GET_ATNAM USING P_ATINN.
  READ TABLE IT_CABN WITH KEY ATINN = P_ATINN.
  IF SY-SUBRC = 0.
    W_ATNAM = IT_CABN-ATNAM.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA.

  DATA: LT_VBRP LIKE TABLE OF IT_VBRP WITH HEADER LINE.
  DATA: L_LEN TYPE I,
        L_MI_9.

** Changed by Furong on 08/20/08
  IF IT_VBRK_CAN[] IS INITIAL.
  ELSE.
    LOOP AT IT_VBRK_CAN.
*      READ TABLE IT_VBRP WITH KEY VBELN = IT_VBRK_CAN-SFAKN.
      LOOP AT IT_VBRP WHERE VBELN = IT_VBRK_CAN-SFAKN.
        DELETE IT_VBRP.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
** End of change

  DESCRIBE TABLE IT_VBRP LINES W_TEMP.

  IF W_TEMP = 0.
    MESSAGE I000 WITH TEXT-M01.
    STOP.
  ENDIF.
  WRITE W_TEMP TO W_CNT USING EDIT MASK 'RR_________'.

  LT_VBRP[] = IT_VBRP[].
  DELETE ADJACENT DUPLICATES FROM LT_VBRP COMPARING VBELN .
  DESCRIBE TABLE LT_VBRP LINES W_TEMP .
  WRITE W_TEMP TO W_BILL USING EDIT MASK 'RR_________'.

  REFRESH : IT_DOWN, IT_R1, IT_R2, IT_R3.
  CLEAR   : IT_DOWN, IT_R1, IT_R2, IT_R3.
* File's Header
  IT_DOWN-RECORD+0(3)   = 'O1H'.
  IT_DOWN-RECORD+3(8)   = SY-DATUM.
  IT_DOWN-RECORD+11(6)  = SY-UZEIT.
  IT_DOWN-RECORD+17(63) = ''.
  APPEND IT_DOWN.


  LOOP AT IT_VBRP.

    SELECT *
         INTO TABLE IT_AUSP
         FROM AUSP
        WHERE OBJEK EQ IT_VBRP-VGBEL  "Vehicle No.
          AND ATINN IN R_ATINN
          AND KLART = '002'.

    LOOP AT IT_AUSP.
      PERFORM GET_ATNAM USING IT_AUSP-ATINN.

      CASE W_ATNAM.
        WHEN 'P_WORK_ORDER'.
          IT_R1-F2+00(02) = '20'.
          IT_R1-F2+02     = IT_AUSP-ATWRT+1(4).
          IT_R1-F3        = IT_AUSP-ATWRT+5(1).
          IT_R1-F4+0(3)   = IT_AUSP-ATWRT+2(3).
          IT_R1-F4+3(3)   = IT_AUSP-ATWRT+6(3).
          IT_R1-F10       = IT_AUSP-ATWRT.
        WHEN 'P_REGION_PORT'.
          IT_R1-F5 = IT_AUSP-ATWRT+0(2).
          IT_R1-F6 = IT_AUSP-ATWRT+2(2).
        WHEN 'P_ORDER_ZONE'.
          IT_R1-F7 = IT_AUSP-ATWRT.
        WHEN 'P_FLEET'.
          IT_R1-F8 = IT_AUSP-ATWRT.
        WHEN 'P_MANUAL_ORDER'.
          IT_R1-F9 = IT_AUSP-ATWRT.
        WHEN 'P_SALES_ORDER'.
          IT_R1-F11 = IT_AUSP-ATWRT.
        WHEN 'P_VIN'.
          IT_R2-F2 = IT_AUSP-ATWRT.
        WHEN 'P_ENGINE_NO'.
          IT_R2-F3 = IT_AUSP-ATWRT.
        WHEN 'P_KEY_NO'.
          IT_R2-F4 = IT_AUSP-ATWRT.
        WHEN 'P_RP18_SHOP_DATE'.
          W_N_8 = IT_AUSP-ATFLV.
          IF W_N_8 NE '00000000'.
            IT_R2-F5+0(04) = W_N_8+0(4).
            IT_R2-F5+4(02) = W_N_8+4(2).
            IT_R2-F5+6(02) = W_N_8+6(2).
          ENDIF.
        WHEN 'P_MODEL_YEAR'.
          IT_R2-F6 = IT_AUSP-ATWRT.
          IT_R2-F7+0(1) = IT_AUSP-ATWRT.
        WHEN 'P_DESTINATION_CODE'.
** Changed on 11/06/07 by Furong for ebom
          IF L_MI_9 IS INITIAL.
            IT_R2-F7+1(5) = IT_AUSP-ATWRT.
          ELSE.
            CLEAR: L_MI_9.
            IT_R2-F7+1(3) = IT_AUSP-ATWRT(3).
            IT_R2-F7+4(1) = IT_AUSP-ATWRT+4(1).
          ENDIF.
*          it_r2-f7+1(5) = it_ausp-atwrt.
** End of change
          IT_R3-F2+0(3) = IT_AUSP-ATWRT+0(3).
          IT_R3-F2+3(2) = IT_VBRP-FKDAT+2(2).
          IT_R3-F2+5(2) = IT_VBRP-FKDAT+4(2).
          IT_R3-F2+7(2) = IT_VBRP-FKDAT+6(2).

          SELECT SINGLE * FROM VBRK WHERE VBELN = IT_VBRP-VBELN.
          IF IT_VBRP-VBTYP  = 'N'.
            IT_VBRP-NETWR = - IT_VBRP-NETWR.
          ENDIF.
          WRITE IT_VBRP-NETWR TO IT_R3-F3 CURRENCY VBRK-WAERK
                              USING EDIT MASK 'RR________V'.
          CLEAR: W_NETPR.
*          WRITE it_vbrp-netwr TO w_netpr CURRENCY vbrk-waerk
*                              USING EDIT MASK 'RR____________V'.
          W_NETPR   = IT_VBRP-NETWR.
          W_AMOUNT = W_AMOUNT + W_NETPR.
          W_DATE = VBRK-FKDAT.
          W_TERM = VBRK-ZTERM.
**>>> Added By Tonkey 06/29/2004.
        WHEN 'P_MI'.
**<<< Added By Tonkey.
**>>> Inactivated By Tonkey 06/29/2004.
*        when 'P_MODEL_INDEX'.
**<<< Inactivated By Tonkey.
          L_LEN = STRLEN( IT_AUSP-ATWRT ).
          IF L_LEN > 7.
            L_MI_9 = 'X'.
            IT_R2-F7+5(9) = IT_AUSP-ATWRT+0(9).
          ELSE.
            IT_R2-F7+6(7) = IT_AUSP-ATWRT+0(7).
          ENDIF.
*          it_r2-f7+6(7) = it_ausp-atwrt+0(7).               """ -1
        WHEN 'P_OCN'.
          IT_R2-F7+14(4) = IT_AUSP-ATWRT.
        WHEN 'P_COLOR_SER'.
*          it_r2-f8 = it_ausp-atwrt.     "changed by chris 02/15/2005
          IT_R1-F4+6(2) = IT_AUSP-ATWRT.
**>>> Added By Tonkey 02/15/2005.
        WHEN 'P_EXT_COLOR'.
          IT_R2-F8 = IT_AUSP-ATWRT.
        WHEN 'P_INT_COLOR'.
          IT_R2-F9 = IT_AUSP-ATWRT.
**>>> End of Add By Tonkey 02/15/2005.
        WHEN 'P_AIRBAG_NO10'.                               "UD1K922885
          IT_R3-F4 = IT_AUSP-ATWRT.                         "UD1K922885
      ENDCASE.
    ENDLOOP.

*   File's Body
    IT_DOWN+0(02)  = 'S1'.     IT_DOWN+2(06)  = IT_R1-F2.
    IT_DOWN+8(01)  = IT_R1-F3. IT_DOWN+9(08)  = IT_R1-F4.
    IT_DOWN+17(02) = IT_R1-F5. IT_DOWN+19(02) = IT_R1-F6.
    IT_DOWN+21(04) = IT_R1-F7. IT_DOWN+25(01) = IT_R1-F8.
    IT_DOWN+26(01) = IT_R1-F9. IT_DOWN+27(10) = IT_R1-F10.
    IT_DOWN+37(10) = IT_R1-F11.
*    append it_down. clear it_down.
    IT_DOWN+47(02)  = 'S2'.     IT_DOWN+49(17)  = IT_R2-F2.
    IT_DOWN+66(15)  = IT_R2-F3. IT_DOWN+81(06)  = IT_R2-F4.

    IT_DOWN+87(08)  = IT_R2-F5. IT_DOWN+95(01)  = IT_R2-F6.
    IT_DOWN+96(18)  = IT_R2-F7. IT_DOWN+114(03) = IT_R2-F8.
    IT_DOWN+117(03) = IT_R2-F9.
    IT_DOWN+120(08) = IT_R3-F3.
    IT_DOWN+128(20) = IT_R2-RDSER.
*    it_down+136(02) = 'S3'.
    IT_DOWN+148(09) = IT_VBRP-VBELN+1(9).
*    it_down+147(13) = it_r3-f3.
* Begin of changes - "UD1K922885 - XM Radio ID
    IT_DOWN+157(30) =    IT_R3-F4 .
* End of changes -   "UD1K922885 -- XM Radio ID
    APPEND IT_DOWN. CLEAR IT_DOWN.
    CLEAR: IT_R1, IT_R2, IT_R3.
  ENDLOOP.

  WRITE W_AMOUNT TO W_AMOUNTC CURRENCY VBRK-WAERK
             USING EDIT MASK 'RR_____________'.

  PERFORM CONVERT_FORMAT USING W_AMOUNTC.

* File's Trailer
  CLEAR IT_DOWN.
  IT_DOWN-RECORD+0(3)   = 'O1T'.
  IT_DOWN-RECORD+3(9)   = W_CNT.
  IT_DOWN-RECORD+12(9)  = W_BILL.
  IT_DOWN-RECORD+21(13) = W_AMOUNTC.
  IT_DOWN-RECORD+34(8)  = W_DATE.
  IT_DOWN-RECORD+42(4)  = W_TERM.
  APPEND IT_DOWN. CLEAR IT_DOWN.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  LOOP AT IT_DOWN.
    WRITE:/ IT_DOWN-RECORD.
  ENDLOOP.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'DOWN'.
      PERFORM DOWNLOAD_DATA.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
FORM DOWNLOAD_DATA.
**  CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**               'HM_' SY-DATUM+2(6)
**               'M.txt'
**               INTO W_DSN.
  IF S_VBELN[] IS INITIAL.
    CONCATENATE  '/usr/sap/EDI_SAP/'
              'HMMAMN_' SY-DATUM+2(6)
              'M.txt'
              INTO W_DSN.
  ELSE.
    CONCATENATE  '/usr/sap/EDI_SAP/'
                 'HMMAMN_' SY-DATUM+2(6)
                 'BL.txt'
                 INTO W_DSN.
  ENDIF.
  OPEN DATASET W_DSN IN TEXT MODE FOR OUTPUT.

  LOOP AT IT_DOWN.

    TRANSFER IT_DOWN TO W_DSN.

  ENDLOOP.

  CLOSE DATASET W_DSN.

  IF SY-SUBRC = 0.
    MESSAGE I000 WITH TEXT-M02.
  ELSE.
    MESSAGE I000 WITH TEXT-M03.
  ENDIF.
ENDFORM.                    " DOWNLOAD_HMA
*&---------------------------------------------------------------------*
*&      Form  convert_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_AMOUNT  text
*----------------------------------------------------------------------*
FORM CONVERT_FORMAT USING    P_AMOUNT.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_AMOUNT
       IMPORTING
            OUTPUT = P_AMOUNT.

ENDFORM.                    " convert_format
