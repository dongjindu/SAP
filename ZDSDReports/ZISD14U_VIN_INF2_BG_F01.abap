*----------------------------------------------------------------------*
*   INCLUDE ZISD14U_VIN_INF2_F01                                       *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  PERFORM GET_ATINN USING 'P_RP18_SHOP_DATE'.

  SELECT *
         INTO TABLE IT_AUSP
         FROM AUSP
        WHERE OBJEK IN ( SELECT OBJEK
                                FROM AUSP
                               WHERE ATINN = W_ATINN
                                 AND KLART = '002'
                                 AND ATFLV IN S_ATFLV )
          AND ATINN IN R_ATINN
          AND KLART = '002'.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_CABN
*&---------------------------------------------------------------------*
FORM INIT_CABN.
  IMPORT S_DATE FROM DATABASE INDX(ZS) ID VARIANT.
  IMPORT P_DES FROM DATABASE INDX(ZS) ID VAR_DES.
  IMPORT P_NEW FROM DATABASE INDX(ZS) ID VAR_NEW.

  REFRESH : IT_CABN, R_ATINN.
  CLEAR   : IT_CABN, R_ATINN.

  R_ATINN-SIGN = 'I'.
  R_ATINN-OPTION = 'EQ'.

  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_VIN'.
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
        WHERE ATNAM = 'P_RP18_SHOP_DATE'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP19_SHOP_DATE'.   "control gate
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP25_SHOP_DATE'.  "Truck out
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_RP27_SHOP_DATE'.  "Rail out
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_WORK_ORDER'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_MANIFEST'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.

  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
*        WHERE atnam = 'P_MANIFEST'.
       WHERE ATNAM = 'P_ENGINE_NO'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_219_28'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_219_7'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_KEY_NO'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
** Chnaged by Furong on 11/08/07
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_TM_NO'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
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
** End of change
** Chnaged by Furong on 03/11/09
  SELECT SINGLE ATINN ATNAM
         INTO (IT_CABN-ATINN, IT_CABN-ATNAM)
         FROM CABN
        WHERE ATNAM = 'P_AIRBAG_NO16'.
  APPEND IT_CABN.
  R_ATINN-LOW = IT_CABN-ATINN. APPEND R_ATINN.
** End of change on 03/11/09


  REFRESH S_ATFLV. CLEAR S_ATFLV.
  LOOP AT S_DATE.
*    MOVE-CORRESPONDING S_DATE TO S_ATFLV.
    S_ATFLV-SIGN   = S_DATE-SIGN.
    S_ATFLV-OPTION = S_DATE-OPTION.
    W_ATFLV = S_DATE-LOW.
    S_ATFLV-LOW = W_ATFLV.

    W_ATFLV = S_DATE-HIGH.
    S_ATFLV-HIGH = W_ATFLV.

    APPEND S_ATFLV. CLEAR S_ATFLV.
  ENDLOOP.
ENDFORM.                    " INIT_CABN
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
  DATA : W_OBJEK LIKE AUSP-OBJEK.
  DATA : L_ATINN LIKE CABN-ATINN.
  DATA:  WA_AUSP LIKE IT_AUSP.

  REFRESH IT_LIST. CLEAR IT_LIST.

  READ TABLE IT_AUSP INDEX 1.
  W_OBJEK = IT_AUSP-OBJEK.
  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
    WHERE ATNAM  = 'P_DESTINATION_CODE'.

** added by furong on 11/10/05 for binary read it_ausp table.
  IT_AUSP_TEMP[] = IT_AUSP[].
  SORT IT_AUSP_TEMP BY ATINN OBJEK.

  LOOP AT IT_AUSP.

*   get the customer
*    READ TABLE IT_AUSP
*       INTO WA_AUSP
*       WITH KEY ATINN = L_ATINN
*                OBJEK = IT_AUSP-OBJEK.
    CLEAR: WA_AUSP.
    READ TABLE IT_AUSP_TEMP
       INTO WA_AUSP
       WITH KEY ATINN = L_ATINN
                OBJEK = IT_AUSP-OBJEK
                BINARY SEARCH.

    IF W_OBJEK <> IT_AUSP-OBJEK.
*Issue #  requested by YKKO
*Changed by wskim,on 01122005
*-----Start
      IT_LIST-F2 = 'B28MM'.
*-----End
      APPEND IT_LIST. CLEAR IT_LIST.
      W_OBJEK = IT_AUSP-OBJEK.
    ENDIF.

    PERFORM GET_ATNAM USING IT_AUSP-ATINN.

    CASE W_ATNAM.
      WHEN 'P_RP27_SHOP_DATE'.
        IF NOT WA_AUSP-ATWRT CS 'B28'.
          W_N_8 = IT_AUSP-ATFLV+0(8).
          IF W_N_8 <> '00000000'.
            IT_LIST-RP19_SHOP_DATE = W_N_8.
          ENDIF.
        ENDIF.
      WHEN 'P_RP25_SHOP_DATE'.
        IF NOT WA_AUSP-ATWRT CS 'B28'.
          W_N_8 = IT_AUSP-ATFLV+0(8).
          IF W_N_8 <> '00000000'.
            IT_LIST-RP19_SHOP_DATE = W_N_8.
          ENDIF.
        ENDIF.
      WHEN 'P_RP19_SHOP_DATE'.
        IF WA_AUSP-ATWRT CS 'B28'.
          W_N_8 = IT_AUSP-ATFLV+0(8).
          IF W_N_8 <> '00000000'.
            IT_LIST-RP19_SHOP_DATE = W_N_8.
          ENDIF.
        ENDIF.
      WHEN 'P_RP18_SHOP_DATE'.
        W_N_8 = IT_AUSP-ATFLV+0(8).
        IT_LIST-RP18_SHOP_DATE = W_N_8.
      WHEN OTHERS.
        CONCATENATE 'IT_LIST-' W_ATNAM+2(20) INTO FIELD.
        ASSIGN (FIELD) TO <FS>.
        <FS> = IT_AUSP-ATWRT.
        IF IT_AUSP-ATWRT = 'B28AB'.
          <FS> = 'B28AA'.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  IF SY-SUBRC = 0.
    IT_LIST-F2 = 'B28MM'.
    APPEND IT_LIST. CLEAR IT_LIST.
  ENDIF.
** end
  DELETE IT_LIST WHERE NOT RP18_SHOP_DATE IN S_DATE.
** Furong on 08/31/12
  DELETE IT_LIST WHERE RP18_SHOP_DATE = '00000000'.
** End
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
FORM DOWNLOAD_DATA.
  DATA : DSN(90).

  DESCRIBE TABLE IT_LIST LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M02.
    STOP.
  ENDIF.

**  CONCATENATE  '/sapmnt/' SY-SYSID '/EDI/'
**               'vin_info_' SY-DATUM
**               '.txt'
**               INTO DSN.
  IF P_DES = 'C'.
    CONCATENATE  '/usr/sap/EDI_SAP/'
                 'vin_info_' SY-DATUM
                 '.txt'
                 INTO DSN.
  ELSE.
    CONCATENATE  '/usr/sap/EDI_SAP/'
                 'hma_vin_info_' SY-DATUM
                 '.txt'
                 INTO DSN.
  ENDIF.

  IF P_NEW = 'X'.
    DATA : BEGIN OF WA_LIST_NEW,
       VIN(17),
       F2(5),
       DESTINATION_CODE(5),
       RP18_SHOP_DATE(8),
       RP19_SHOP_DATE(8),
       F6(20),
       WORK_ORDER(15),
** Changed by Furong on 11/08/07
*       MANIFEST(11),
       ENGINE_NO(15),
** End of change
       219_28(1),
       219_7(1),
       F11(1),
       KEY_NO(5),
       TM_NO(15),
       EXT_COLOR(3),
       INT_COLOR(3),
       AIRBAG_NO16(14),
       END OF WA_LIST_NEW.
    OPEN DATASET DSN IN TEXT MODE FOR OUTPUT.
    LOOP AT IT_LIST.
      MOVE-CORRESPONDING  IT_LIST TO WA_LIST_NEW.
      OPEN DATASET DSN IN TEXT MODE FOR APPENDING.
      TRANSFER WA_LIST_NEW TO DSN.
    ENDLOOP.
    CLOSE DATASET DSN.
  ELSE.
    DATA : BEGIN OF WA_LIST_OLD,
         VIN(17),
         F2(5),
         DESTINATION_CODE(5),
         RP18_SHOP_DATE(8),
         RP19_SHOP_DATE(8),
         F6(20),
         WORK_ORDER(15),
** Changed by Furong on 11/08/07
         MANIFEST(11),
** End of change
         219_28(1),
         219_7(1),
         F11(1),
         KEY_NO(5),
         END OF WA_LIST_OLD.
    OPEN DATASET DSN IN TEXT MODE FOR OUTPUT.
    LOOP AT IT_LIST.
      MOVE-CORRESPONDING  IT_LIST TO WA_LIST_OLD.
      OPEN DATASET DSN IN TEXT MODE FOR APPENDING.
      TRANSFER WA_LIST_OLD TO DSN.
    ENDLOOP.
    CLOSE DATASET DSN.
  ENDIF.

  IF SY-SUBRC = 0.
    MESSAGE I000 WITH TEXT-M03.
    IF W_CNT > 1.
      WRITE: /10 W_CNT ,
              25 'RECORDS ARE DOWNLOADED.'.
    ELSE.
      WRITE: /10 W_CNT ,
              25 'RECORD IS DOWNLOADED.'.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH TEXT-M04.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
