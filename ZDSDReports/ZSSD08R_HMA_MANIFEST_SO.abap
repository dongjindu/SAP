************************************************************************
* Program Name      : ZSSD08R_HMA_MANIFEST
* Author            : Crhis Li
* Creation Date     : 2004.01.24.
* Specifications By : Chris Li
* Pattern           : 5-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : HMA MANIFEST.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zssd08r_hma_manifest NO STANDARD PAGE HEADING
                            LINE-SIZE 200
                            MESSAGE-ID zmsd.


*
TABLES : ausp, cabn, vbfa, vbrk, vbrp, vbap, LIKP.


*
DATA : BEGIN OF it_ausp OCCURS 0.
        INCLUDE STRUCTURE ausp.
DATA : END OF it_ausp.

data : it_objek like it_ausp occurs 0 with header line.

DATA : BEGIN OF it_vbrp OCCURS 0,
         vbeln LIKE vbrp-vbeln,  "Billing document
         fkdat LIKE vbrk-fkdat,  "Billing date
         vgbel LIKE vbrp-vgbel,  "Vehicle No.
        "Net value of the billing item in document currency
         netwr LIKE vbrp-netwr,
         zterm like vbrk-zterm,  "payment terms
         vbtyp LIKE vbrk-fktyp,  " UD1K913940
         fksto LIKE vbrk-fksto,  " UD1K913940
         sfakn LIKE vbrk-sfakn,  " UD1K913940
         shkzg LIKE vbrp-shkzg,  " UD1K913940
       END OF it_vbrp.

RANGES r_atinn FOR ausp-atinn OCCURS 0.

DATA : BEGIN OF it_cabn OCCURS 0,
       atinn LIKE cabn-atinn,
       atnam LIKE cabn-atnam,
       END OF it_cabn.

DATA : BEGIN OF it_down OCCURS 0,
       record(150),
       END OF it_down.

DATA : BEGIN OF it_r1 OCCURS 0,
       f1(02),
       f2(06),  "YYYYMM
       f3(01),
       f4(08),
       f5(02),
       f6(02),
       f7(04),
       f8(01),
       f9(01),
       f10(14),
       f11(10),
       END OF it_r1.

DATA : BEGIN OF it_r2 OCCURS 0,
       f1(02),
       f2(18),
       f3(12),
       f4(06),
       f5(08),
       f6(01),
       f7(17),
       f8(03),
       F9(03),
       END OF it_r2.

DATA : BEGIN OF it_r3 OCCURS 0,
       f1(02),
       f2(09),
       f3(13),
       END OF it_r3.

DATA : ok_code(4),
       save_ok_code(4).

DATA : w_bill(4) TYPE n,  "The Total of Billing Document
       w_cnt(9)  TYPE P DECIMALS 0,  "The Total of Vehicle
       W_COUNT(9) TYPE C,
       w_date(8),         "invoice date
       w_term(4),         "payment terms
       w_netpr(13),       "Net price
       w_amount(13),      "Total amount
       w_atinn LIKE cabn-atinn,
       w_atnam LIKE cabn-atnam,
       w_n_8(8) TYPE n,
       w_objek LIKE ausp-objek,
       " W_DSN_B(20) VALUE '/usr/sap/EDI_SAP/',
       w_dsn(90).


*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
parameters:      r_so radiobutton group rb1.
selection-screen comment: (30) text-003 for field r_so.
selection-screen end of line.
SELECTION-SCREEN BEGIN OF LINE.
selection-screen comment: 5(20) text-004 for field r_so.
SELECT-OPTIONS : s_vbeln   for vbap-vbeln.
selection-screen end of line.
selection-screen skip.
SELECTION-SCREEN BEGIN OF LINE.
parameters:      r_dd radiobutton group rb1.
selection-screen comment: 5(30) text-002 for field r_dd.
selection-screen end of line.
SELECTION-SCREEN BEGIN OF LINE.
selection-screen comment: 5(20) text-005 for field r_so.
SELECT-OPTIONS : S_DELIV    FOR LIKP-VBELN.
selection-screen end of line.
SELECTION-SCREEN END OF BLOCK b1.


*
START-OF-SELECTION.
  SET PF-STATUS 'SSD08R'.
  PERFORM init_cabn.
  PERFORM get_data.
  PERFORM modify_data.
  PERFORM display_data.


*
END-OF-SELECTION.


*
AT USER-COMMAND.
  PERFORM user_command.


*&---------------------------------------------------------------------*
*&      Form  INIT_CABN
*&---------------------------------------------------------------------*
FORM init_cabn.
  REFRESH : it_cabn, r_atinn.
  CLEAR   : it_cabn, r_atinn.

  r_atinn-sign = 'I'.
  r_atinn-option = 'EQ'.

  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_WORK_ORDER'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_REGION_PORT'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_ORDER_ZONE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_FLEET'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_MANUAL_ORDER'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_SALES_ORDER'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_VIN'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_ENGINE_NO'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_KEY_NO'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP18_SHOP_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_MODEL_YEAR'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_DESTINATION_CODE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
**>>> Added By Tonkey 06/29/2004.
         WHERE atnam = 'P_MI'.
**<<< Added By Tonkey.
**>>> Inactivated By Tonkey 06/29/2004.
*        where atnam = 'P_MODEL_INDEX'.
**<<< Inactivated By Tonkey.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_OCN'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_COLOR_SER'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
* changed by chris: adding p_ext_color p_int_color
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_EXT_COLOR'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_INT_COLOR'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP25_ACURAL_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.
  SELECT SINGLE atinn atnam
         INTO (it_cabn-atinn, it_cabn-atnam)
         FROM cabn
        WHERE atnam = 'P_RP27_ACURAL_DATE'.
  APPEND it_cabn.
  r_atinn-low = it_cabn-atinn. APPEND r_atinn.




ENDFORM.                    " INIT_CABN
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  data: l_atinn like cabn-atinn.
  DATA: L_ATINN1 LIKE CABN-ATINN.

  select single atinn into l_atinn
      from cabn
      where atnam = 'P_SALES_ORDER'.
    select single atinn into l_atinn1
      from cabn
      where atnam = 'P_RP_STATUS'.

  if r_so = 'X'.

* get the vehicle for the sales order

    SELECT * INTO TABLE IT_OBJEK
      FROM AUSP
      WHERE OBJEK IN ( SELECT OBJEK FROM AUSP
                      WHERE KLART = '002'
                        AND ATINN = L_ATINN1
                        AND ATWRT GE '18' )
       AND KLART = '002'
       AND ATINN = L_ATINN
       AND ATWRT IN S_VBELN.

   ELSE.
*   get the vehicle from delivery document.
*   delevery document number is the vehicle number
    select * into table it_objek
      from ausp
      where objek in s_deliv
        AND KLART  = '002'
        AND ATINN  = L_ATINN1
        AND ATWRT  GE '18'.


   ENDIF.

  ENDFORM.                    " GET_DATA


*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
FORM get_atinn USING p_atnam.
  READ TABLE it_cabn WITH KEY atnam = p_atnam.
  IF sy-subrc = 0.
    w_atinn = it_cabn-atinn.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  GET_ATNAM
*&---------------------------------------------------------------------*
FORM get_atnam USING p_atinn.
  READ TABLE it_cabn WITH KEY atinn = p_atinn.
  IF sy-subrc = 0.
    w_atnam = it_cabn-atnam.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data.

  DATA: lt_AUSP LIKE TABLE OF AUSP WITH HEADER LINE.
  data: l_netwr like vbrk-netwr.

  DESCRIBE TABLE it_OBJEK LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE i000 WITH text-m01.
    STOP.
  ENDIF.


  DESCRIBE TABLE IT_OBJEK LINES w_bill .

  REFRESH : it_down, it_r1, it_r2, it_r3.
  CLEAR   : it_down, it_r1, it_r2, it_r3.
* File's Header
  it_down-record+0(3)   = 'O1H'.
  it_down-record+3(8)   = sy-datum.
  it_down-record+11(6)  = sy-uzeit.
  it_down-record+17(63) = ''.
  APPEND it_down.


  LOOP AT it_OBJEK.
    SELECT *
         INTO TABLE it_ausp
         FROM ausp
        WHERE objek EQ it_OBJEK-OBJEK  "Vehicle No.
          AND atinn IN r_atinn
          AND klart = '002'.

    LOOP AT it_ausp.
      PERFORM get_atnam USING it_ausp-atinn.

      CASE w_atnam.
        WHEN 'P_WORK_ORDER'.
          it_r1-f2+00(02) = '20'.
          it_r1-f2+02     = it_ausp-atwrt+1(4).
          it_r1-f3        = it_ausp-atwrt+5(1).
          it_r1-f4+0(3)   = it_ausp-atwrt+2(3).
          it_r1-f4+3(3)   = it_ausp-atwrt+6(3).
          it_r1-f10       = it_ausp-atwrt.
        WHEN 'P_REGION_PORT'.
          it_r1-f5 = it_ausp-atwrt+0(2).
          it_r1-f6 = it_ausp-atwrt+2(2).
        WHEN 'P_ORDER_ZONE'.
          it_r1-f7 = it_ausp-atwrt.
        WHEN 'P_FLEET'.
          it_r1-f8 = it_ausp-atwrt.
        WHEN 'P_MANUAL_ORDER'.
          it_r1-f9 = it_ausp-atwrt.
        WHEN 'P_SALES_ORDER'.
          it_r1-f11 = it_ausp-atwrt.
        WHEN 'P_VIN'.
          it_r2-f2 = it_ausp-atwrt.
        WHEN 'P_ENGINE_NO'.
          it_r2-f3 = it_ausp-atwrt.
        WHEN 'P_KEY_NO'.
          it_r2-f4 = it_ausp-atwrt.
        WHEN 'P_RP18_SHOP_DATE'.
          w_n_8 = it_ausp-atflv.
          IF w_n_8 NE '00000000'.
            it_r2-f5+0(04) = w_n_8+0(4).
            it_r2-f5+4(02) = w_n_8+4(2).
            it_r2-f5+6(02) = w_n_8+6(2).
          ENDIF.
        WHEN 'P_MODEL_YEAR'.
          it_r2-f6 = it_ausp-atwrt.
          it_r2-f7+0(1) = it_ausp-atwrt.
        WHEN 'P_DESTINATION_CODE'.
          it_r2-f7+1(5) = it_ausp-atwrt.
          it_r3-f2+0(3) = it_ausp-atwrt+0(3).


*   because this type manifest no billing no charge so
*   the price is 0
*    set the delivery date
          select single * from likp
             where vbeln  = it_objek-objek
              and  vbtyp  = 'J'.
          clear: l_netwr.
          WRITE l_netwr TO it_r3-f3 CURRENCY vbrk-waerk
                              USING EDIT MASK 'RR____________V'.

          CLEAR: w_netpr.
          WRITE l_netwr TO w_netpr CURRENCY vbrk-waerk
                              USING EDIT MASK 'RR____________V'.
          w_amount = w_amount + w_netpr.
          w_date = likp-lfdat.


        WHEN 'P_MI'.
          it_r2-f7+6(7) = it_ausp-atwrt+0(7).               """ -1
        WHEN 'P_OCN'.
          it_r2-f7+13(4) = it_ausp-atwrt.
        WHEN 'P_MI'.
          it_r1-f4+6(2) = it_ausp-atwrt.
        WHEN 'P_EXT_COLOR'.
          it_r2-f8 = it_ausp-atwrt.
        WHEN 'P_INT_COLOR'.
          it_r2-f9 = it_ausp-atwrt.
        WHEN 'P_RP25_ACTURAL_DATE'.
          it_r3-f2+3(2) = it_ausp-atwrt+2(2).
          it_r3-f2+5(2) = it_ausp-atwrt+4(2).
          it_r3-f2+7(2) = it_ausp-atwrt+6(2).
        WHEN 'P_RP27_ACTUAL_DATE'.
          it_r3-f2+3(2) = it_ausp-atwrt+2(2).
          it_r3-f2+5(2) = it_ausp-atwrt+4(2).
          it_r3-f2+7(2) = it_ausp-atwrt+6(2).

      ENDCASE.
    ENDLOOP.

*   File's Body
    it_down+0(02)  = 'S1'.     it_down+2(06)  = it_r1-f2.
    it_down+8(01)  = it_r1-f3. it_down+9(08)  = it_r1-f4.
    it_down+17(02) = it_r1-f5. it_down+19(02) = it_r1-f6.
    it_down+21(04) = it_r1-f7. it_down+25(01) = it_r1-f8.
    it_down+26(01) = it_r1-f9. it_down+27(14) = it_r1-f10.
    it_down+41(10) = it_r1-f11.
*    append it_down. clear it_down.
    it_down+51(02)  = 'S2'.     it_down+53(18)  = it_r2-f2.
    it_down+71(12)  = it_r2-f3. it_down+83(06)  = it_r2-f4.
    it_down+89(08)  = it_r2-f5. it_down+97(01)  = it_r2-f6.
    it_down+98(17)  = it_r2-f7. it_down+115(03) = it_r2-f8.
    it_down+118(03) = it_r2-f9.
*    append it_down. clear it_down.
    it_down+121(02) = 'S3'.     it_down+123(09) = it_vbrp-vbeln+1(9).
    it_down+132(13) = it_r3-f3.
    APPEND it_down. CLEAR it_down.
    CLEAR: it_r1, it_r2,it_r3.             " UD1K913940
  ENDLOOP.

* File's Trailer
  CLEAR it_down.
  it_down-record+0(3)   = 'O1T'.
  WRITE W_CNT TO W_COUNT USING EDIT MASK 'RR_________'.
  it_down-record+3(9)   = w_COUNT.
  it_down-record+12(8)  = w_date.
*  it_down-record+20(4)  = w_term.
  APPEND it_down. CLEAR it_down.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data.
  LOOP AT it_down.
    WRITE:/ it_down-record.
  ENDLOOP.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'DOWN'.
      PERFORM download_data.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
FORM download_data.
**  CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**               'HM_' SY-DATUM+2(6)
**               'M.txt'
**               INTO W_DSN.
  CONCATENATE  '/usr/sap/EDI_SAP/'
               'HINV_' SY-DATUM '_MD.TXT'
               INTO w_dsn.

  OPEN DATASET w_dsn IN TEXT MODE FOR output.
  if sy-subrc ne 0.
    message i000 with 'File open error'.
    exit.
  endif.
  LOOP AT it_down.
    TRANSFER it_down TO w_dsn.
  ENDLOOP.

  CLOSE DATASET w_dsn.

  IF sy-subrc = 0.
    MESSAGE i000 WITH text-m02.
  ELSE.
    MESSAGE i000 WITH text-m03.
  ENDIF.
ENDFORM.                    " DOWNLOAD_HMA
