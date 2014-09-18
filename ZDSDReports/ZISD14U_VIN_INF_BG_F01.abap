*----------------------------------------------------------------------*
*   INCLUDE ZISD14U_VIN_INF_F01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  IMPORT S_DATE FROM DATABASE INDX(ZS) ID VARIANT.

  PERFORM GET_ATINN.

  SELECT OBJEK
  INTO   TABLE IT_OBJEK
  FROM   AUSP
  WHERE  ATINN EQ L_ATINN4
  AND    ATWRT IN S_DATE.

  SELECT OBJEK ATINN ATWRT ATFLV
  INTO   TABLE ITAB1
  FROM   AUSP
  FOR ALL ENTRIES IN IT_OBJEK
  WHERE  ATINN IN  (L_ATINN1, L_ATINN3,  L_ATINN4,
                    L_ATINN7, L_ATINN8,  L_ATINN9,
                    L_ATINN10, L_ATINN12, L_ATINN13,
                    L_ATINN14, L_ATINN15)
  AND   OBJEK EQ IT_OBJEK-OBJEK.

  SORT ITAB1 BY OBJEK.

  DATA: L_OBJEK LIKE AUSP-OBJEK.

**          F1(17), "VIN
**          F2(5),  "MANUFACURER
**          F3(5),  "DISTRUBUTOR CODE
**          F4(8),  "PRODUCTION DATE
**          F5(8),  "SHIPPING DATE
**          F6(20), "VEHICLE SPEC
**          F7(15), "WORK ORDER NUMBER
**          F8(11), "ENGINE NUMBER
**          F9(1),  "AIR/CON CODE
**          F10(1), "TRANSMISSION CODE
**          F11(1), "SPARE FLAG
**          F12(5), "KEY NUMBER
  DATA: L_LINES TYPE I,
        L_COUNT TYPE I.

  ITAB3[] = ITAB1[].

  DESCRIBE TABLE ITAB1 LINES L_LINES.

  CLEAR: ITAB1, L_OBJEK.
  LOOP AT ITAB1.
    L_COUNT = SY-TABIX.

    IF L_OBJEK IS INITIAL.
      L_OBJEK = ITAB1-OBJEK.
    ELSE.
      IF L_OBJEK EQ ITAB1-OBJEK.
        IF L_LINES = L_COUNT.
          PERFORM GET_CHAR_VALUE.
          ITAB2-F2 = 'B28MM'.
          ITAB2-F6 = ''.
          ITAB2-F11 = ''.

          IF ITAB2-F3 = 'B28AA'.
            READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
                                      ATINN = L_ATINN13.
            IF SY-SUBRC = 0.
              W_N_8 = ITAB3-ATFLV.
              ITAB2-F5 = W_N_8.
*****         ITAB2-F5 = ITAB3-ATWRT.
            ENDIF.
          ELSE.
            READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
                                      ATINN = L_ATINN14.
            IF SY-SUBRC = 0.
              W_N_8 = ITAB3-ATFLV.
              ITAB2-F5 = W_N_8.
*****         ITAB2-F5 = ITAB3-ATWRT.
            ENDIF.

            IF ITAB3-ATWRT IS INITIAL.
              READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
                                        ATINN = L_ATINN15.
              IF SY-SUBRC = 0.
                W_N_8 = ITAB3-ATFLV.
                ITAB2-F5 = W_N_8.
*****           ITAB2-F5 = ITAB3-ATWRT.
              ENDIF.
            ENDIF.
          ENDIF.

          APPEND ITAB2.
          L_OBJEK = ITAB1-OBJEK.
          CLEAR: ITAB2.
        ENDIF.
      ELSE.
        ITAB2-F2 = 'B28MM'.
        ITAB2-F6 = ''.
        ITAB2-F11 = ''.

        IF ITAB2-F3 = 'B28AA'.
          READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
                                    ATINN = L_ATINN13.
          IF SY-SUBRC = 0.
            W_N_8 = ITAB3-ATFLV.
            ITAB2-F5 = W_N_8.
*****       ITAB2-F5 = ITAB3-ATWRT.
          ENDIF.
        ELSE.
          READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
                                    ATINN = L_ATINN14.
          IF SY-SUBRC = 0.
            W_N_8 = ITAB3-ATFLV.
            ITAB2-F5 = W_N_8.
*****       ITAB2-F5 = ITAB3-ATWRT.
          ENDIF.

          IF ITAB3-ATWRT IS INITIAL.
            READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
                                      ATINN = L_ATINN15.
            IF SY-SUBRC = 0.
              W_N_8 = ITAB3-ATFLV.
              ITAB2-F5 = W_N_8.
*****         ITAB2-F5 = ITAB3-ATWRT.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND ITAB2.
        L_OBJEK = ITAB1-OBJEK.
        CLEAR: ITAB2.
      ENDIF.
    ENDIF.

    PERFORM GET_CHAR_VALUE.

    CLEAR: ITAB1.
  ENDLOOP.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_SHIPPING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SHIPPING_DATE.

*        ITAB2-F2 = 'B28MM'.
*        ITAB2-F6 = ''.
*        ITAB2-F11 = ''.
*
*        IF ITAB2-F3 = 'B28AA'.
*          READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
*                                    ATINN = L_ATINN13.
*          IF SY-SUBRC = 0.
*            ITAB2-F5 = ITAB3-ATWRT.
*          ENDIF.
*        ELSE.
*          READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
*                                    ATINN = L_ATINN14.
*          IF SY-SUBRC = 0.
*            ITAB2-F5 = ITAB3-ATWRT.
*          ENDIF.
*
*          IF ITAB3-ATWRT IS INITIAL.
*            READ TABLE ITAB3 WITH KEY OBJEK = L_OBJEK
*                                      ATINN = L_ATINN15.
*            IF SY-SUBRC = 0.
*              ITAB2-F5 = ITAB3-ATWRT.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*        APPEND ITAB2.
*        L_OBJEK = ITAB1-OBJEK.
*        CLEAR: ITAB2.
ENDFORM.                    " GET_SHIPPING_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_CHAR_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CHAR_VALUE.

  IF ITAB1-ATINN EQ L_ATINN1.
    MOVE ITAB1-ATWRT TO ITAB2-F1.
*    ELSEIF ITAB1-ATINN EQ L_ATINN2.
  ELSEIF ITAB1-ATINN EQ L_ATINN3.
    IF ITAB1-ATWRT = 'B28AB'.
      ITAB1-ATWRT = 'B28AA'.
    ENDIF.
    MOVE ITAB1-ATWRT TO ITAB2-F3.

  ELSEIF ITAB1-ATINN EQ L_ATINN4.
    W_N_8 = ITAB1-ATFLV.
    ITAB2-F4 = W_N_8.
****MOVE ITAB1-ATWRT TO ITAB2-F4.

*    ELSEIF ITAB1-ATINN EQ L_ATINN5.
*    ELSEIF ITAB1-ATINN EQ L_ATINN6.
  ELSEIF ITAB1-ATINN EQ L_ATINN7.
    MOVE ITAB1-ATWRT TO ITAB2-F7.

  ELSEIF ITAB1-ATINN EQ L_ATINN8.
    MOVE ITAB1-ATWRT TO ITAB2-F8.

  ELSEIF ITAB1-ATINN EQ L_ATINN9.
    MOVE ITAB1-ATWRT TO ITAB2-F9.

  ELSEIF ITAB1-ATINN EQ L_ATINN10.
    MOVE ITAB1-ATWRT TO ITAB2-F10.
*    ELSEIF ITAB1-ATINN EQ L_ATINN11.
  ELSEIF ITAB1-ATINN EQ L_ATINN12.
    MOVE ITAB1-ATWRT TO ITAB2-F10.

  ELSE.

  ENDIF.

ENDFORM.                    " GET_CHAR_VALUE
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ATINN.

  SELECT SINGLE ATINN
         INTO L_ATINN1
         FROM CABN
         WHERE ATNAM = 'P_VIN'.

  SELECT SINGLE ATINN
        INTO L_ATINN3
        FROM CABN
        WHERE ATNAM = 'P_DESTINATION_CODE'.

  SELECT SINGLE ATINN
         INTO L_ATINN4
         FROM CABN
         WHERE ATNAM = 'P_RP18_SHOP_DATE'.

  SELECT SINGLE ATINN
         INTO L_ATINN7
         FROM CABN
         WHERE ATNAM = 'P_WORK_ORDER'.

  SELECT SINGLE ATINN
         INTO L_ATINN8
         FROM CABN
         WHERE ATNAM = 'P_MANIFEST'.

  SELECT SINGLE ATINN
         INTO L_ATINN9
         FROM CABN
         WHERE ATNAM = 'P_219_28'.

  SELECT SINGLE ATINN
         INTO L_ATINN10
         FROM CABN
         WHERE ATNAM = 'P_219_7'.

  SELECT SINGLE ATINN
         INTO L_ATINN12
         FROM CABN
         WHERE ATNAM = 'P_KEY_NO'.

***** GET SHIPPING DATE

  SELECT SINGLE ATINN
         INTO L_ATINN13
         FROM CABN
         WHERE ATNAM = 'P_RP21_SHOP_DATE'.

  SELECT SINGLE ATINN
         INTO L_ATINN14
         FROM CABN
         WHERE ATNAM = 'P_RP24_SHOP_DATE'.

  SELECT SINGLE ATINN
         INTO L_ATINN15
         FROM CABN
         WHERE ATNAM = 'P_RP26_SHOP_DATE'.

ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_DATA.
  DATA : DSN(90),
         L_LINE TYPE I.

  DESCRIBE TABLE ITAB2 LINES L_LINE.
  IF L_LINE = 0.
    MESSAGE I000 WITH TEXT-M02.
    STOP.
  ENDIF.

**  CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**               'vin_info_' SY-DATUM
**               '.txt'
**               INTO DSN.
  CONCATENATE  '/usr/sap/EDI_SAP/'
               'vin_info_' SY-DATUM
               '.txt'
               INTO DSN.

  LOOP AT ITAB2.
    OPEN DATASET DSN IN TEXT MODE FOR APPENDING.
    TRANSFER ITAB2 TO DSN.
  ENDLOOP.

  CLOSE DATASET DSN.

  IF SY-SUBRC = 0.
    MESSAGE I000 WITH TEXT-M03.
    IF L_LINE > 1.
      WRITE: /10 L_LINE ,
              25 'RECORDS ARE DOWNLOADED.'.
    ELSE.
      WRITE: /10 L_LINE ,
              25 'RECORD IS DOWNLOADED.'.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M04.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
