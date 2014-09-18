*----------------------------------------------------------------------*
*   INCLUDE MZPP_APPLICATIONI01                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  LEAVE PROGRAM.
  FREE MEMORY ID 'ZWORKN'.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_100 INPUT.
  DATA: RETURN_CODE TYPE I.

  SV_CODE = OK_CODE.
  CLEAR: OK_CODE.

  CASE SV_CODE.
    WHEN SPACE .
    WHEN 'BACK'. " Finish program
      SV_PROG  = SY-REPID .
      SV_DYNNR = '0001'   .
      SV_CODE  = '0001'   .
      PERFORM CLEAR_COMMON_VAL .
      PERFORM CLEAR_SCREEN_VALS.
    WHEN '0101' OR '0102' OR '0103' OR '0104' OR '0105' OR '0106' OR
         '0107' OR '0108' OR '0109' OR '0110' OR '0111' OR '0118' OR
                   '0701' OR '1201' OR '1202' OR '1203' OR '1205' OR
         '1206' OR '1209' OR '1210' OR '2101' OR '2102' OR '2103' OR
         '2104' OR '2105' OR '2106' OR '2107' OR '2108' OR '2109' OR
         '2110' OR '2111' OR '2113' OR '2114' OR '2115' OR '2116' OR
         '2117' OR '2118' OR '2119' OR '2120' OR '2121' OR '2122' OR
         '2123' OR '2124' OR '2200' OR '2201' OR '2202' OR '2203' OR
         '2204' OR '2205' OR '2206' OR '2207' OR '2301' OR '3104' OR
         '3107' OR '3109' OR '3211' OR '3301' OR '3302' OR '3303' OR
         '4101' OR '4102' OR '4103' OR '4104' OR '8081' OR '8082' OR
         '8088'.

      SV_DYNNR = SV_CODE .
      PERFORM CLEAR_COMMON_VAL .
*     PERFORM CLEAR_TABLES     .
      PERFORM CLEAR_SCREEN_VALS.
  ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001 INPUT.
  IF WA_PROD = SPACE.  CLEAR: WA_PROD_DATE.  ENDIF.
  CASE OK_CODE.
    WHEN 'BACK' .
      PERFORM CHECK_SAVE_1001  .
    WHEN SPACE  .
    WHEN 'RCODE'.
      PERFORM DISPLAY_1001     .
      PERFORM DISPLAY_1001_219 .
    WHEN 'NCODE'.
      PERFORM GET_NEXT_DISPLAY .
      PERFORM DISPLAY_1001     .
      PERFORM DISPLAY_1001_219 .
    WHEN 'SCHNG'.
      IF WA_CHG_1001_FLG = ' ' .
        WA_CHG_1001_FLG = 'X'    .
        WA_SAVE_FLG     = 'X'    .
      ELSE.
        WA_CHG_1001_FLG = ' '  .
      ENDIF.
    WHEN 'SAVE' .
      PERFORM SAVE_1001        .
      WA_CHG_1001_FLG = ' '    .
      WA_SAVE_FLG     = ' '    .
    WHEN 'DEXCEL'.
      PERFORM DOWNLOAD_1001    .
    WHEN 'R219' .
      PERFORM DISPLAY_1001_219 .
*    WHEN OTHERS .
*      CLEAR: wa_wosum[], wa_wosum, it_219, it_219[],
*             it_wosum, it_wosum[].
  ENDCASE.
  DESCRIBE TABLE IT_219 LINES TC_0101-LINES.
ENDMODULE.                 " USER_COMMAND_1001  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1002 INPUT.
  CASE OK_CODE.
    WHEN 'RCODE'.
      PERFORM DISPLAY_1002     .
    WHEN 'NCODE'.
      PERFORM GET_NEXT_HEADER_1002.
*      perform get_next_color   .
      PERFORM DISPLAY_1002     .
    WHEN 'DEXCEL'.
      PERFORM DOWNLOAD_1002    .
    WHEN OTHERS .
  ENDCASE.
  DESCRIBE TABLE IT_WOSUM LINES TC_0102-LINES.
ENDMODULE.                 " USER_COMMAND_1002  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1003 INPUT.
  CASE OK_CODE.
    WHEN 'RCODE'.
      PERFORM DISPLAY_1003     .
    WHEN 'NCODE'.
      PERFORM GET_NEXT_DISPLAY .
      PERFORM DISPLAY_1003     .
    WHEN 'OK1'   .
      IF WA_OK1_FLAG = 'X'.
        WA_1003_CHK01 = WA_1003_CHK02 = WA_1003_CHK03 = ' ' .
        WA_1003_CHK04 = WA_1003_CHK05 = WA_1003_CHK06 = ' ' .
        WA_1003_CHK07 = WA_1003_CHK08 = WA_1003_CHK09 = ' ' .
        WA_1003_CHK10 = WA_1003_CHK11 = WA_1003_CHK12 = ' ' .
        WA_1003_CHK13 = WA_OK1_FLAG = ' '.
      ELSE.
        WA_1003_CHK01 = WA_1003_CHK02 = WA_1003_CHK03 = 'X' .
        WA_1003_CHK04 = WA_1003_CHK05 = WA_1003_CHK06 = 'X' .
        WA_1003_CHK07 = WA_1003_CHK08 = WA_1003_CHK09 = 'X' .
        WA_1003_CHK10 = WA_1003_CHK11 = WA_1003_CHK12 = 'X' .
        WA_1003_CHK13 = WA_OK1_FLAG = 'X'.
      ENDIF.
    WHEN 'OK2'   .
      IF WA_OK2_FLAG = 'X'.
        WA_1003_CHK25 = WA_1003_CHK14 = WA_1003_CHK15 = ' ' .
        WA_1003_CHK16 = WA_1003_CHK17 = WA_1003_CHK18 = ' ' .
        WA_1003_CHK19 = WA_1003_CHK20 = WA_1003_CHK21 = ' ' .
        WA_1003_CHK22 = WA_1003_CHK23 = WA_1003_CHK24 = ' ' .
        WA_1003_CHK26 = WA_OK2_FLAG = ' '.
      ELSE.
        WA_1003_CHK25 = WA_1003_CHK14 = WA_1003_CHK15 = 'X' .
        WA_1003_CHK16 = WA_1003_CHK17 = WA_1003_CHK18 = 'X' .
        WA_1003_CHK19 = WA_1003_CHK20 = WA_1003_CHK21 = 'X' .
        WA_1003_CHK22 = WA_1003_CHK23 = WA_1003_CHK24 = 'X' .
        WA_1003_CHK26 = WA_OK2_FLAG = 'X'.
      ENDIF.
    WHEN 'OK3'   .
      IF WA_OK3_FLAG = 'X'.
        WA_1003_CHK37 = WA_1003_CHK38 = WA_1003_CHK27 = ' ' .
        WA_1003_CHK28 = WA_1003_CHK29 = WA_1003_CHK30 = ' ' .
        WA_1003_CHK31 = WA_1003_CHK32 = WA_1003_CHK33 = ' ' .
        WA_1003_CHK34 = WA_1003_CHK35 = WA_1003_CHK36 = ' ' .
        WA_1003_CHK39 = WA_OK3_FLAG = ' '.
      ELSE.
        WA_1003_CHK37 = WA_1003_CHK38 = WA_1003_CHK27 = 'X' .
        WA_1003_CHK28 = WA_1003_CHK29 = WA_1003_CHK30 = 'X' .
        WA_1003_CHK31 = WA_1003_CHK32 = WA_1003_CHK33 = 'X' .
        WA_1003_CHK34 = WA_1003_CHK35 = WA_1003_CHK36 = 'X' .
        WA_1003_CHK39 = WA_OK3_FLAG = 'X'.
      ENDIF.
    WHEN 'OK4'   .
      IF WA_OK4_FLAG = 'X'.
        WA_1003_CHK40 = WA_1003_CHK41 = WA_1003_CHK42 = ' ' .
        WA_1003_CHK43 = WA_1003_CHK44 = WA_1003_CHK45 = ' ' .
        WA_1003_CHK46 = WA_1003_CHK47 = WA_1003_CHK48 = ' ' .
        WA_1003_CHK49 = WA_1003_CHK50 =                 ' ' .
        WA_OK4_FLAG = ' '.
      ELSE.
        WA_1003_CHK40 = WA_1003_CHK41 = WA_1003_CHK42 = 'X' .
        WA_1003_CHK43 = WA_1003_CHK44 = WA_1003_CHK45 = 'X' .
        WA_1003_CHK46 = WA_1003_CHK47 = WA_1003_CHK48 = 'X' .
        WA_1003_CHK49 = WA_1003_CHK50 =                 'X' .
        WA_OK4_FLAG = 'X'.
      ENDIF.
    WHEN 'SAVE'  .
      WA_SAVE_FLG = ' '.
    WHEN 'SCHNG' .
      WA_SAVE_FLG = 'X'.
    WHEN 'DEXCEL'.
      PERFORM DOWNLOAD_1003    .
    WHEN OTHERS .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1003  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1004 INPUT.
  CASE OK_CODE.
    WHEN 'RCODE'.
      PERFORM DISPLAY_1004     .
    WHEN 'NCODE'.
      PERFORM GET_NEXT_DISPLAY .
      PERFORM DISPLAY_1004     .
    WHEN 'OK1'   .
      IF WA_OK1_FLAG = 'X'.
        WA_1004_CHK01 = WA_1004_CHK02 = WA_1004_CHK03 = ' ' .
        WA_1004_CHK04 = WA_1004_CHK05 = WA_1004_CHK06 = ' ' .
        WA_1004_CHK07 = WA_1004_CHK08 = WA_1004_CHK09 = ' ' .
        WA_1004_CHK10 = WA_1004_CHK11 = WA_1004_CHK12 = ' ' .
        WA_1004_CHK13 = WA_OK1_FLAG = ' '.
      ELSE.
        WA_1004_CHK01 = WA_1004_CHK02 = WA_1004_CHK03 = 'X' .
        WA_1004_CHK04 = WA_1004_CHK05 = WA_1004_CHK06 = 'X' .
        WA_1004_CHK07 = WA_1004_CHK08 = WA_1004_CHK09 = 'X' .
        WA_1004_CHK10 = WA_1004_CHK11 = WA_1004_CHK12 = 'X' .
        WA_1004_CHK13 = WA_OK1_FLAG = 'X'.
      ENDIF.
    WHEN 'OK2'   .
      IF WA_OK2_FLAG = 'X'.
        WA_1004_CHK25 = WA_1004_CHK14 = WA_1004_CHK15 = ' ' .
        WA_1004_CHK16 = WA_1004_CHK17 = WA_1004_CHK18 = ' ' .
        WA_1004_CHK19 = WA_1004_CHK20 = WA_1004_CHK21 = ' ' .
        WA_1004_CHK22 = WA_1004_CHK23 = WA_1004_CHK24 = ' ' .
        WA_1004_CHK26 = WA_OK2_FLAG = ' '.
      ELSE.
        WA_1004_CHK25 = WA_1004_CHK14 = WA_1004_CHK15 = 'X' .
        WA_1004_CHK16 = WA_1004_CHK17 = WA_1004_CHK18 = 'X' .
        WA_1004_CHK19 = WA_1004_CHK20 = WA_1004_CHK21 = 'X' .
        WA_1004_CHK22 = WA_1004_CHK23 = WA_1004_CHK24 = 'X' .
        WA_1004_CHK26 = WA_OK2_FLAG = 'X'.
      ENDIF.
    WHEN 'OK3'   .
      IF WA_OK3_FLAG = 'X'.
        WA_1004_CHK37 = WA_1004_CHK38 = WA_1004_CHK27 = ' ' .
        WA_1004_CHK28 = WA_1004_CHK29 = WA_1004_CHK30 = ' ' .
        WA_1004_CHK31 = WA_1004_CHK32 = WA_1004_CHK33 = ' ' .
        WA_1004_CHK34 = WA_1004_CHK35 = WA_1004_CHK36 = ' ' .
        WA_1004_CHK39 = WA_OK3_FLAG = ' '.
      ELSE.
        WA_1004_CHK37 = WA_1004_CHK38 = WA_1004_CHK27 = 'X' .
        WA_1004_CHK28 = WA_1004_CHK29 = WA_1004_CHK30 = 'X' .
        WA_1004_CHK31 = WA_1004_CHK32 = WA_1004_CHK33 = 'X' .
        WA_1004_CHK34 = WA_1004_CHK35 = WA_1004_CHK36 = 'X' .
        WA_1004_CHK39 = WA_OK3_FLAG = 'X'.
      ENDIF.
    WHEN 'OK4'   .
      IF WA_OK4_FLAG = 'X'.
        WA_1004_CHK40 = WA_1004_CHK41 = WA_1004_CHK42 = ' ' .
        WA_1004_CHK43 = WA_1004_CHK44 = WA_1004_CHK45 = ' ' .
        WA_1004_CHK46 = WA_1004_CHK47 = WA_1004_CHK48 = ' ' .
        WA_1004_CHK49 = WA_1004_CHK50 =                 ' ' .
        WA_OK4_FLAG = ' '.
      ELSE.
        WA_1004_CHK40 = WA_1004_CHK41 = WA_1004_CHK42 = 'X' .
        WA_1004_CHK43 = WA_1004_CHK44 = WA_1004_CHK45 = 'X' .
        WA_1004_CHK46 = WA_1004_CHK47 = WA_1004_CHK48 = 'X' .
        WA_1004_CHK49 = WA_1004_CHK50 =                 'X' .
        WA_OK4_FLAG = 'X'.
      ENDIF.
    WHEN 'SAVE'.
      WA_SAVE_FLG = ' ' .
    WHEN 'SCHNG' .
      WA_SAVE_FLG = 'X' .
    WHEN 'DEXCEL'.
      PERFORM DOWNLOAD_1004    .
    WHEN OTHERS .

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1004  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1005 INPUT.
  CASE OK_CODE.
    WHEN 'RCODE'.
      PERFORM DISPLAY_1005     .
    WHEN 'NCODE'.
      PERFORM GET_NEXT_DISPLAY .
      PERFORM DISPLAY_1005     .
    WHEN 'OK1'   .
      IF WA_OK1_FLAG = 'X'.
        CLEAR: IT_ALCU_A-CHECK .
        MODIFY IT_ALCU_A TRANSPORTING CHECK WHERE CHECK = 'X' .
        WA_OK1_FLAG = 'X'.
      ELSE.
        IT_ALCU_A-CHECK = 'X'.
        MODIFY IT_ALCU_A TRANSPORTING CHECK WHERE CHECK = ' ' .
        WA_OK1_FLAG = 'X'.
      ENDIF.
    WHEN 'OK2'   .
      IF WA_OK2_FLAG = 'X'.
        CLEAR: IT_ALCU_B-CHECK .
        MODIFY IT_ALCU_B TRANSPORTING CHECK WHERE CHECK = 'X' .
        WA_OK2_FLAG = ' '.
      ELSE.
        IT_ALCU_B-CHECK = 'X'.
        MODIFY IT_ALCU_B TRANSPORTING CHECK WHERE CHECK = ' ' .
        WA_OK2_FLAG = 'X'.
      ENDIF.
    WHEN 'OK3'   .
      IF WA_OK3_FLAG = 'X'.
        CLEAR: IT_ALCU_C-CHECK .
        MODIFY IT_ALCU_C TRANSPORTING CHECK WHERE CHECK = 'X' .
        WA_OK3_FLAG = ' '.
      ELSE.
        IT_ALCU_C-CHECK = 'X'.
        MODIFY IT_ALCU_C TRANSPORTING CHECK WHERE CHECK = ' ' .
        WA_OK3_FLAG = 'X'.
      ENDIF.
    WHEN 'OK4'   .
      IF WA_OK4_FLAG = 'X'.
        CLEAR: IT_ALCU_D-CHECK .
        MODIFY IT_ALCU_D TRANSPORTING CHECK WHERE CHECK = 'X' .
        WA_OK4_FLAG = ' '.
      ELSE.
        IT_ALCU_D-CHECK = 'X'.
        MODIFY IT_ALCU_D TRANSPORTING CHECK WHERE CHECK = ' ' .
        WA_OK4_FLAG = 'X'.
      ENDIF.
    WHEN 'SCHNG' .
    WHEN 'DEXCEL'.
      PERFORM DOWNLOAD_0105    .
    WHEN OTHERS .
  ENDCASE.
  DESCRIBE TABLE IT_ALCU_A LINES TC_A105-LINES.
  DESCRIBE TABLE IT_ALCU_B LINES TC_B105-LINES.
  DESCRIBE TABLE IT_ALCU_C LINES TC_C105-LINES.
  DESCRIBE TABLE IT_ALCU_D LINES TC_D105-LINES.
ENDMODULE.                 " USER_COMMAND_1005  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1006 INPUT.
  CASE OK_CODE.
    WHEN 'RCODE'.
      PERFORM DISPLAY_1005     .
    WHEN 'NCODE'.
      PERFORM GET_NEXT_DISPLAY .
      PERFORM DISPLAY_1005     .
    WHEN 'OK1'   .
      IF WA_OK1_FLAG = 'X'.
        CLEAR: IT_ALCU_A-CHECK .
        MODIFY IT_ALCU_A TRANSPORTING CHECK WHERE CHECK = 'X' .
        WA_OK1_FLAG = 'X'.
      ELSE.
        IT_ALCU_A-CHECK = 'X'.
        MODIFY IT_ALCU_A TRANSPORTING CHECK WHERE CHECK = ' ' .
        WA_OK1_FLAG = 'X'.
      ENDIF.
    WHEN 'OK2'   .
      IF WA_OK2_FLAG = 'X'.
        CLEAR: IT_ALCU_B-CHECK .
        MODIFY IT_ALCU_B TRANSPORTING CHECK WHERE CHECK = 'X' .
        WA_OK2_FLAG = ' '.
      ELSE.
        IT_ALCU_B-CHECK = 'X'.
        MODIFY IT_ALCU_B TRANSPORTING CHECK WHERE CHECK = ' ' .
        WA_OK2_FLAG = 'X'.
      ENDIF.
    WHEN 'OK3'   .
      IF WA_OK3_FLAG = 'X'.
        CLEAR: IT_ALCU_C-CHECK .
        MODIFY IT_ALCU_C TRANSPORTING CHECK WHERE CHECK = 'X' .
        WA_OK3_FLAG = ' '.
      ELSE.
        IT_ALCU_C-CHECK = 'X'.
        MODIFY IT_ALCU_C TRANSPORTING CHECK WHERE CHECK = ' ' .
        WA_OK3_FLAG = 'X'.
      ENDIF.
    WHEN 'OK4'   .
      IF WA_OK4_FLAG = 'X'.
        CLEAR: IT_ALCU_D-CHECK .
        MODIFY IT_ALCU_D TRANSPORTING CHECK WHERE CHECK = 'X' .
        WA_OK4_FLAG = ' '.
      ELSE.
        IT_ALCU_D-CHECK = 'X'.
        MODIFY IT_ALCU_D TRANSPORTING CHECK WHERE CHECK = ' ' .
        WA_OK4_FLAG = 'X'.
      ENDIF.
    WHEN 'SCHNG' .
    WHEN 'DEXCEL'.
      PERFORM DOWNLOAD_0106    .
    WHEN OTHERS .
  ENDCASE.
  DESCRIBE TABLE IT_ALCU_A LINES TC_A106-LINES.
  DESCRIBE TABLE IT_ALCU_B LINES TC_B106-LINES.
  DESCRIBE TABLE IT_ALCU_C LINES TC_C106-LINES.
  DESCRIBE TABLE IT_ALCU_D LINES TC_D106-LINES.
ENDMODULE.                 " USER_COMMAND_1006  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2001 INPUT.
  SV_CODE = OK_CODE.
  CLEAR: OK_CODE   .
  CASE SV_CODE.
    WHEN 'RCODE'.
      PERFORM DISPLAY_2001     .
    WHEN 'NCODE'.
      PERFORM GET_NEXT_DISPLAY .
      PERFORM DISPLAY_2001     .
    WHEN 'DEXCEL'.
    WHEN OTHERS .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2001  INPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_0101 INPUT.

ENDMODULE.                 " MODIFY_0101  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2101 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM EMISSION_DATA_SELECTION.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_2101' OK_CODE.
    WHEN 'DEXCEL'.
      PERFORM DOWN_LOADING_EXECL_2101.
    WHEN 'SCHNG'.
      PERFORM CHANGE_2101_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2101  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2102 INPUT.
  CASE OK_CODE.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_2102'  OK_CODE.
    WHEN 'ZDIS'.
      PERFORM CAL_REGION_PRODUCTION_SELECTIO.
    WHEN 'ZEXL'.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2102  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2103 INPUT.
  CASE OK_CODE.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_2103'  OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_2103.
    WHEN 'ZEXL'.

    WHEN 'PICK'.
      PERFORM DATA_PICK.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2103  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_2104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2104 INPUT.
  CASE OK_CODE.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_2104'  OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_2104.
    WHEN 'ZBACK'.
      PERFORM SCREEN_BACK_2103.
    WHEN 'ZCHG'.
      PERFORM DATA_CHANGE_2104.
    WHEN 'ZNEXT'.
      PERFORM DATA_NEXT_100_ENTR.
  ENDCASE.
ENDMODULE.                 " user_command_2104  INPUT

*&---------------------------------------------------------------------*
*&      Module  IT_2104_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_2104_PAI INPUT.
  MODIFY IT_2104  INDEX TC_2104-CURRENT_LINE.

ENDMODULE.                 " IT_2104_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2202 INPUT.
  CASE OK_CODE.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_2202'  OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_2202.
    WHEN 'SCHNG'.
      IF WA_CHANGE = ' '.
        WA_CHANGE = 'X' .
      ELSE.
        WA_CHANGE = ' ' .
      ENDIF.
    WHEN 'ZEXLC'.

    WHEN 'DELT' .
      PERFORM DELT_VEHICLE_2202.
      PERFORM DATA_SELECT_2202.
    WHEN 'SAVE' .
      PERFORM SAVE_VEHICLE_2202.
      PERFORM DATA_SELECT_2202.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2202  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2203  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2203 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_2203 .
    WHEN 'ZNEX'.
      PERFORM INCREASE_VM_2203 .
      CHECK WA_ERR_FLAG = SPACE .
      PERFORM DATA_SELECT_2203 .
    WHEN 'SCHNG'.
      IF WA_CHANGE = ' '.
        WA_CHANGE = 'X' .
      ELSE.
        WA_CHANGE = ' ' .
      ENDIF.
    WHEN 'SAVE' .
      PERFORM SAVE_VEHICLE_2202.
    WHEN 'ZINS'.
      PERFORM DATA_INSERT_RETURN_2203 .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2203  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2204  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2204 INPUT.
  DATA: W_FLAG_SCRAP(1).
  CASE OK_CODE.
    WHEN 'ZDIS'.  "Search Data!
      PERFORM DATA_SELECT_2204 .
    WHEN 'ZNEX'.  "The Next VM No.
      PERFORM SET_NEXT_VM_APP260 .
      CHECK WA_ERR_FLAG = SPACE  .
      PERFORM DATA_SELECT_2204 .
*      perform data_next_entry_2204 .
*   WHEN 'ZFIRST'.  "The First VM No.
*     PERFORM data_first_entry_2204 .
*   WHEN 'ZNDAT'.  "The Next Date
*     PERFORM data_select_2204 .
*     perform data_next_date_2204 .
    WHEN 'SCHNG'.
      IF WA_CHANGE = SPACE.
        WA_CHANGE = 'X' .
      ELSE.
        WA_CHANGE = ' ' .
      ENDIF.
    WHEN 'SAVE' OR 'ZINT'.
      IF ST_2204-SCRAP = SPACE OR ST_2204-USAGE = SPACE OR
         ST_2204-TEXT  = SPACE .
        MESSAGE S001 WITH TEXT-012 .
      ELSE.
*         IF pa_USAGE_TYPE = 'S' and lt_vals-atwrt >= '18'.
*            EXIT.
*        endif.
*
*  IF pa_USAGE_TYPE = 'D' and lt_vals-atwrt < '19'.
*     EXIT.
*  endif.
        PERFORM DATA_CHANGE_ENTRY_2204 .
        IF W_FLAG_SCRAP = 'X'.
          LEAVE PROGRAM.
        ENDIF.
      ENDIF.
    WHEN 'ZLIST'.  "List
      PERFORM LIST_2205.
    WHEN  'ZAPS'.
      IF ST_2204_INPUT-DATE IS INITIAL.
        SUBMIT ZIPP112I_APS_3AA1_2 WITH P_DATE = SY-DATUM
                                   WITH P_SCRP = 'X' AND RETURN.
      ELSE.
        SUBMIT ZIPP112I_APS_3AA1_2 WITH P_DATE = SY-DATUM
                               WITH P_SCRP = 'X' AND RETURN.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2204  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2205  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2205 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_2205 .
      DESCRIBE TABLE IT_2205 LINES TC_2205-LINES.
    WHEN 'ZEXL'.
      PERFORM EXCEL_DOWN_2205 .
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_2205'  OK_CODE.
*   WHEN 'PICK'.
*     PERFORM use_pick_list.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2205  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2206  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2206 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_2206 .
    WHEN 'ZEXL'.
      PERFORM EXCEL_DOWN_2205 .
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_2206' OK_CODE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2206  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0109  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0109 INPUT.

  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_0109 .
      DESCRIBE TABLE IT_0109 LINES WA_LINES .
      IF WA_LINES = 0.
        MESSAGE S001 WITH TEXT-100.
      ENDIF.
    WHEN 'ZEXL'.
      PERFORM EXCEL_DOWN_0109 .
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_0109'
                                             OK_CODE.
    WHEN 'ZEXCL'.
      PERFORM DOWNLOAD_0109.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0109  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1007 INPUT.
  CASE OK_CODE.
    WHEN 'RCODE'.
      CLEAR OK_CODE.
      PERFORM CLEAR_PARAMETERS_APP207 .
      PERFORM DISPLAY_APP207     .
    WHEN 'NCODE'.
      CLEAR OK_CODE.
      PERFORM GET_NEXT_ERROR   .
      PERFORM DISPLAY_1007     .
    WHEN 'R219'  .
      CLEAR OK_CODE.
      PERFORM DISPLAY_1001_219 .
      DESCRIBE TABLE IT_219 LINES TC_0107-LINES.
    WHEN 'SCHNG' .
    WHEN 'DEXCEL'.
      PERFORM DOWNLOAD_0107    .
    WHEN 'CS'.
      PERFORM CLEAR_PARAMETERS_APP207 .
      PERFORM READ_WO_INF_1007 USING WA_WO_PACK .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1007  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1008  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1008 INPUT.
  CASE OK_CODE.
    WHEN 'RCODE'.
      PERFORM DISPLAY_1007     .
    WHEN 'NCODE'.
      PERFORM GET_NEXT_ERROR   .
      PERFORM DISPLAY_1007     .
    WHEN 'R219'  .
      PERFORM DISPLAY_1001_219 .
      DESCRIBE TABLE IT_219 LINES TC_0107-LINES.
    WHEN 'SCHNG' .
    WHEN 'DEXCEL'.
*     perform download_1005    .
    WHEN OTHERS .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1008  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0110 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_0110 .
    WHEN 'ZNEX'.
      PERFORM DATA_SELECT_0110 .
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_0110'  OK_CODE.
    WHEN 'ZEXCL'.
      PERFORM DOWNLOAD_0110.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0110  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0111 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
*     Display
      PERFORM CLEAR_TABLES_APP211.
      PERFORM SEARCH_TWO_WO_INF_APP211 .
*      PERFORM data_select_0111 .
    WHEN 'ZDIF'.
*     Difference
      PERFORM CLEAR_TABLES_APP211.
      PERFORM SEARCH_TWO_WO_INF_APP211.
      PERFORM DELETE_SAME_DATA_APP211.
*      PERFORM data_difference_0111.
    WHEN 'ZNRX'.
*     Next Order(Header)
      PERFORM SET_NEXT_HEADER_APP211.
      PERFORM CLEAR_TABLES_APP211.
      PERFORM SEARCH_TWO_WO_INF_APP211.
*      PERFORM data_next_selection_0111.
    WHEN 'ZNRC'.
*     Next Color
      PERFORM SET_NEXT_COLOR_APP211.
      PERFORM CLEAR_TABLES_APP211.
      PERFORM SEARCH_TWO_WO_INF_APP211.
*      PERFORM data_next_color_0111.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_0111'  OK_CODE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0111  INPUT

*&---------------------------------------------------------------------*
*&      Module  color_input_check_0111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COLOR_INPUT_CHECK_0111 INPUT.
* input check.
  IF ST_0111_INPUT-EXCLR1 <> '' AND  ST_0111_INPUT-INCLR1 = ' '.
    MESSAGE E000(ZMPP) WITH 'Color not input'(101).
  ENDIF.

  IF ST_0111_INPUT-INCLR1 <> '' AND  ST_0111_INPUT-EXCLR1 = ' '.
    MESSAGE E000(ZMPP) WITH 'Color not input'(101).
  ENDIF.

  IF ST_0111_INPUT-EXCLR2 <> '' AND  ST_0111_INPUT-INCLR2 = ' '.
    MESSAGE E000(ZMPP) WITH 'Color not input'(101).
  ENDIF.

  IF ST_0111_INPUT-INCLR2 <> '' AND  ST_0111_INPUT-EXCLR2 = ' '.
    MESSAGE E000(ZMPP) WITH 'Color not input'(101).
  ENDIF.
ENDMODULE.                 " color_input_check_0111  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0118  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0118 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_0118 .
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_0118'  OK_CODE.

*Requested by Hur,20041020,changed by wskim
*-----Start
    WHEN 'PICK'.
      IF SY-DYNNR = 118.
        CLEAR OK_CODE.
        GET CURSOR LINE TC_0118-CURRENT_LINE.
        TC_0118-CURRENT_LINE = TC_0118-CURRENT_LINE +
                            TC_0118-TOP_LINE - 1.
        READ TABLE IT_0118 INDEX TC_0118-CURRENT_LINE.
        FREE MEMORY ID 'ZWORKN'.
        SET PARAMETER ID 'ZWORKN' FIELD IT_0118-ORDER.
        CALL SCREEN 100.
        CLEAR IT_0118-ORDER.
      ENDIF.
*----End
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0118  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_4279  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_4279 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_2479 .
      DESCRIBE TABLE IT_4279  LINES TC_4279-LINES.
      IF TC_4279-LINES = 0.
        MESSAGE W001 WITH TEXT-100.
      ENDIF.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_4279'  OK_CODE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_4279  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5290  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_5290 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_MAKE_5290 .
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_5290'  OK_CODE.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      IF SY-DYNNR = '6301'.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_5290  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5291  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_5291 INPUT.
  CASE OK_CODE.

    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_5291 .

    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_5291'
                                             OK_CODE.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_5291  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5293  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_5293 INPUT.
  CASE OK_CODE.

    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_5293 .

    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_5293'
                                             OK_CODE.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_5293  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_6299  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_6299 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_6299 .
      DESCRIBE TABLE IT_6299 LINES TC_6299-LINES.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_6299'  OK_CODE.
    WHEN 'SORT_UP'  .
      PERFORM SORT_SCREEN_4103  USING 'A'.
    WHEN 'SORT_DOWN'.
      PERFORM SORT_SCREEN_4103  USING 'D'.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_6299  INPUT

*&---------------------------------------------------------------------*
*&      Module  DISP_1001_PROD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISP_1001_PROD INPUT.
  CASE WA_PROD.
    WHEN 'Y'  .
      CLEAR: WA_PROD_DATE, WA_PROD.
    WHEN 'N'  .
      WA_PROD_DATE = 'Holding'.
      WA_PROD      = 'O'      .
    WHEN 'O'  .
      WA_PROD_DATE = 'Holding'.
    WHEN 'D'  .
      WA_PROD_DATE = 'Delete'.
    WHEN SPACE.
      CLEAR: WA_PROD_DATE .
  ENDCASE.
ENDMODULE.                 " DISP_1001_PROD  INPUT

*&---------------------------------------------------------------------*
*&      Module  CAR_DESC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CAR_DESC INPUT.
  DATA: L_FSC                LIKE ZTPP_WOSUM-FSC.

  SELECT SINGLE FSC INTO L_FSC
    FROM ZTPP_WOSUM
   WHERE WO_SER = WA_ORDER(9)
     AND NATION = WA_ORDER+9(3)
     AND DEALER = WA_ORDER+12(2).

  CHECK SY-SUBRC = 0.
  SELECT SINGLE MAKTX
         INTO WA_CAR
         FROM MAKT
         WHERE MATNR EQ L_FSC
           AND SPRAS EQ SY-LANGU.
ENDMODULE.                 " CAR_DESC  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*     Modification of Internal Table with Table Control's Currnet line
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_2200 INPUT.
  MODIFY IT_APP250 INDEX TC_APP250-CURRENT_LINE.
ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_2200  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2200 INPUT.
  CASE OK_CODE.
    WHEN 'SEL'.
      PERFORM SELECT_DATA_2200.
    WHEN 'EXC'.
      PERFORM PROCESS_DOWNLOAD_2200.
    WHEN 'SORTA'. "SORTING ASCENDING.
      PERFORM SORT_SCREEN_2200  USING 'A'.
    WHEN 'SORTD'. "SORTING DESCENDING.
      PERFORM SORT_SCREEN_2200  USING 'D'.
  ENDCASE.
ENDMODULE.                 " user_command_2200  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*     Modification of Internal Table With table Control's Current Line
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_2201 INPUT.
  MODIFY IT_APP252 INDEX TC_APP252-CURRENT_LINE.
ENDMODULE.                 " modify_data  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_2201  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2201 INPUT.
  CASE OK_CODE.
    WHEN 'SEL'.
      PERFORM SELECT_DATA_2201.
      CLEAR OK_CODE.
    WHEN 'EXC'.
      PERFORM PROCESS_DOWNLOAD_2201.
      CLEAR OK_CODE.
    WHEN 'SORTA'. "SORTING ASCENDING.
      PERFORM SORT_SCREEN_2201  USING 'A' .
      CLEAR OK_CODE.
    WHEN 'SORTD'. "SORTING DESCENDING.
      PERFORM SORT_SCREEN_2201  USING 'D' .
      CLEAR OK_CODE.
  ENDCASE.
ENDMODULE.                 " user_command_2201  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1205  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1205 INPUT.
  DATA L_COUNT TYPE SY-TABIX.
  CASE OK_CODE.
    WHEN 'SRH'.  "SEARCH
*     Check Essential conditions
      PERFORM CHECK_ESSENTIAL_CONDITION_1205.
*     Search Data
      PERFORM SEARCH_DATA_1205.
      CLEAR OK_CODE.
    WHEN 'UPD'.  "TOGGLE
      WA_UPD_1205 = 'X'.
      CLEAR OK_CODE.
    WHEN 'SAV'.  "SAVE
      CLEAR WA_UPD_1205.
      CLEAR OK_CODE.
      READ TABLE IT_APP223 WITH KEY MARK = 'X'.
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.
*     Update Data
      PERFORM UPDATE_ALC_1205.
*     Check Essential Conditions
      PERFORM CHECK_ESSENTIAL_CONDITION_1205.
*     Search Data
      PERFORM SEARCH_DATA_1205.
    WHEN 'DEL'.  "DELETE
      CLEAR OK_CODE.
*     Delete Data
      PERFORM DELETE_DATA_1205.
*     Check Essential Conditions
      PERFORM CHECK_ESSENTIAL_CONDITION_1205.
*     Search Data
      PERFORM SEARCH_DATA_1205.
    WHEN 'EXL'.  "EXCEL
*     Set Data For Downloading
      PERFORM SET_EXCEL_1205.
*     Call A Function For Downloading
      PERFORM CALL_FUNC_DOWNLOAD_1205.
      CLEAR OK_CODE.
    WHEN 'FILE'.  "LOOK FOR PATH
      CLEAR OK_CODE.
    WHEN 'BCK'.  "BACK COLUMN
      CLEAR OK_CODE.
      P_KEY = P_KEY - 1.
      IF P_KEY <= 0.
        P_KEY = 1.
        MESSAGE I000 WITH 'There is not the key 0.'.
      ENDIF.
*     Check Essential Conditions
      PERFORM CHECK_ESSENTIAL_CONDITION_1205.
*     Search Data
      PERFORM SEARCH_DATA_1205.
      DESCRIBE TABLE IT_APP223 LINES L_COUNT.
      IF L_COUNT = 0.
        DO.
          P_KEY = P_KEY - 1.
*         Check Essential Conditions
          PERFORM CHECK_ESSENTIAL_CONDITION_1205.
*         Search Data
          PERFORM SEARCH_DATA_1205.
          DESCRIBE TABLE IT_APP223 LINES L_COUNT.
          IF L_COUNT <> 0.
            EXIT.
          ENDIF.
          IF P_KEY = 0.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    WHEN 'NXT'.  "NEXT COLUMN
      CLEAR OK_CODE.
      P_KEY = P_KEY + 1.
*     Check Essential Conditions
      PERFORM CHECK_ESSENTIAL_CONDITION_1205.
*     Search Data
      PERFORM SEARCH_DATA_1205.
*     Read Table count
      DESCRIBE TABLE IT_APP223 LINES L_COUNT.
      IF L_COUNT = 0.
        DO.
          P_KEY = P_KEY + 1.
*         Check Essential Conditions
          PERFORM CHECK_ESSENTIAL_CONDITION_1205.
*         Search Data
          PERFORM SEARCH_DATA_1205.
          DESCRIBE TABLE IT_APP223 LINES L_COUNT.
          IF L_COUNT <> 0.
            EXIT.
          ENDIF.
          IF P_PART = 'C' AND P_KEY > 50.
            EXIT.
          ELSEIF P_KEY > 200.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    WHEN 'ADD'.  "CREATE NEW DATA
      CLEAR: OK_CODE, WA_FLAG.
      CALL SCREEN '1206'. " STARTING AT 10 3 ENDING AT 90 22.
    WHEN 'UPDA'.  "UPDATE ALL
      CLEAR OK_CODE.
    WHEN 'SORTA'. "SORTING ASCENDING.
      CLEAR OK_CODE.
*     Sort By Ascending
      PERFORM SORT_SCREEN_1205  USING 'A'.
    WHEN 'SORTD'. "SORTING DESCENDING.
      CLEAR OK_CODE.
*     Sort By Descending
      PERFORM SORT_SCREEN_1205  USING 'D'.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1205  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       Modification of Internal Table
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_1205 INPUT.
  MODIFY IT_APP223 INDEX TC_APP223-CURRENT_LINE.

ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  back  INPUT
*&---------------------------------------------------------------------*
*       Setting Command - Back
*----------------------------------------------------------------------*
MODULE BACK INPUT.
  LEAVE TO SCREEN 0.

ENDMODULE.                 " back  INPUT
*  CREATE OBJECT g_application.
*&---------------------------------------------------------------------*
*&      Module  initial_value_set_1202  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE_SET_1202 OUTPUT.
  IF IS219-NAME219 IS INITIAL.
    MOVE  '001'      TO   IS219-NAME219.
  ENDIF.
  IF WA_FLAG IS INITIAL .
    WA_FLAG = 'X'.
    NAME = 'IS219-MODEL'.
    PERFORM SET_FIELD_MODEL USING NAME  IS219-MODEL .
  ENDIF.
ENDMODULE.                 " initial_value_set_1202  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  modify_internal_table  INPUT
*&---------------------------------------------------------------------*
*     Modification of Internal Table with Table Control's Current line
*----------------------------------------------------------------------*
MODULE MODIFY_INTERNAL_TABLE_1206 INPUT.
  MODIFY IT_APP223_NEW INDEX TC_APP223_NEW-CURRENT_LINE.
  IF SY-SUBRC <> 0.
    APPEND IT_APP223_NEW.
  ENDIF.

ENDMODULE.                 " modify_internal_table  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1206  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1206 INPUT.
  CASE OK_CODE.
    WHEN 'SAVE'.
      CLEAR OK_CODE.
*     Check Essential Conditions
      PERFORM CHECK_ESSENTIAL_CONDITION_1205.
*     Create New Data
      PERFORM CREATE_NEW_DATA_1206.
      EXIT.
    WHEN 'CLEAR'.
      CLEAR OK_CODE.
*     Clear Internal Table
      PERFORM CLEAR_IT_APP223_NEW.
    WHEN 'BACK'.
      CLEAR OK_CODE.
      SET SCREEN 0100.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      CLEAR OK_CODE.
*     Set Parameters
      PERFORM SETTING_PARAMETERS_1206.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1206  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1209  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands - Main Screen
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1209 INPUT.
  CASE OK_CODE.
*
    WHEN 'RLS'.  "RELEASE.
      PERFORM RELEASE_1209.
      CLEAR OK_CODE.
*
    WHEN 'SEL'.  "SEARCH.
      PERFORM SETUP_PARAMETER_1209.
      PERFORM SELECT_DATA_1209.
      CLEAR OK_CODE.
*
    WHEN 'ADD'.  "CREATE NEW DATA.
      CLEAR OK_CODE.

      CALL SCREEN '1210' STARTING AT 10 3 ENDING AT 70 22.
*
    WHEN 'DLT'.  "DELETE.
      PERFORM DELETE_DATA_1209.
      PERFORM SETUP_PARAMETER_1209.
      PERFORM SELECT_DATA_1209.
      CLEAR OK_CODE.
*
    WHEN 'EXL'.  "DOWNLOAD.
      PERFORM MAKE_FILE_1209.
      PERFORM DOWNLOAD_1209.
      CLEAR OK_CODE.

    WHEN 'SORTA'. "SORTING ASCENDING.
      PERFORM SORT_ASCENDING_1209.
      CLEAR OK_CODE.

    WHEN 'SORTD'. "SORTING DESCENDING.
      PERFORM SORT_DESCENDING_1209.
      CLEAR OK_CODE.
  ENDCASE.
ENDMODULE.                 " user_command_1209  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1210  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands - Sub Screen
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1210 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      CLEAR OK_CODE.
      CLEAR WA_FLAG.
      SET SCREEN '0100'.
      LEAVE TO SCREEN 0.
    WHEN 'CLEAR'.
      CLEAR IT_NEW_APP227.
      REFRESH IT_NEW_APP227.
      CLEAR IT_ERROR_1210.
      REFRESH IT_ERROR_1210.
    WHEN 'SAVE'.
      PERFORM SAVE_NEW_DATA_1210.
      CLEAR OK_CODE.
  ENDCASE.
ENDMODULE.                 " user_command_1210  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_WO  INPUT
*&---------------------------------------------------------------------*
*       Modifying Internal Table with Table Control's Current Line
*----------------------------------------------------------------------*
MODULE CHECK_WO_1210 INPUT.
  PERFORM CHECK_WORK_ORDER_1210 USING IT_NEW_APP227-FORDER.
  MODIFY IT_NEW_APP227 INDEX TC_NEW_APP227-CURRENT_LINE.
*  if sy-subrc <> 0.
*    append it_new_app227.
*  endif.

ENDMODULE.                 " CHECK_WO  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       Modifying Internal Table with Table Control's Current Line
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_1209 INPUT.
  MODIFY IT_APP227 INDEX TC_APP227-CURRENT_LINE.
  IF SY-SUBRC <> 0.
    APPEND IT_APP227 .
  ENDIF.

ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  worder_search_help  INPUT
*&---------------------------------------------------------------------*
*       Setting Search-Help Command - Work Order
*----------------------------------------------------------------------*
MODULE WORDER_SEARCH_HELP_1209 INPUT.
  CLEAR REASON_TAB. REFRESH REASON_TAB.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = 'P_WORDER'.
  APPEND DYNPFIELDS.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = SY-REPID
            DYNUMB               = SY-DYNNR
            DETERMINE_LOOP_INDEX = 'X'
       TABLES
            DYNPFIELDS           = DYNPFIELDS
       EXCEPTIONS
            OTHERS               = 9.

  READ TABLE DYNPFIELDS WITH KEY FIELDNAME = 'P_WORDER'.
* -- WORK ORDER SEARCH
  DATA: L_PLANT(04),
        L_MODEL(06),
        L_WORDER(20).

  CLEAR: L_PLANT, L_MODEL, L_WORDER.
  CONCATENATE WA_PLANT '%' INTO L_PLANT.
  CONCATENATE WA_MODEL '%' INTO L_MODEL.
  CONCATENATE P_WORDER '%' INTO L_WORDER.

  SELECT DISTINCT WORDER
    INTO REASON_TAB-CODE
    FROM ZTPP_SPEC
    WHERE PLANT LIKE L_PLANT AND
          MODEL LIKE L_MODEL AND
          WORDER LIKE L_WORDER .
    APPEND REASON_TAB.
  ENDSELECT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD    = 'CODE'
            DYNPPROG    = SY-CPROG
            DYNPNR      = SY-DYNNR
            DYNPROFIELD = 'P_WORDER'
            VALUE_ORG   = 'S'
       TABLES
            VALUE_TAB   = REASON_TAB.

  IF SY-SUBRC  <> 0.
    MESSAGE I000 WITH 'NOT FOUND....'.
  ENDIF.
ENDMODULE.                 " worder_search_help  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data_110  INPUT
*&---------------------------------------------------------------------*
*       Modifying Internal Table with Table Control's Current Line
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_1210 INPUT.
  MODIFY IT_NEW_APP227 INDEX TC_NEW_APP227-CURRENT_LINE.

  IF SY-SUBRC <> 0.
    APPEND IT_NEW_APP227 .
  ENDIF.

ENDMODULE.                 " modify_data_110  INPUT
*&---------------------------------------------------------------------*
*&      Module  wo_c_search_help  INPUT
*&---------------------------------------------------------------------*
*       Setting Search-Help Command - Work Order
*----------------------------------------------------------------------*
MODULE WO_C_SEARCH_HELP INPUT.
  CLEAR REASON_TAB. REFRESH REASON_TAB.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = 'IT_NEW_APP227-FORDER'.
  APPEND DYNPFIELDS.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = SY-REPID
            DYNUMB               = SY-DYNNR
            DETERMINE_LOOP_INDEX = 'X'
       TABLES
            DYNPFIELDS           = DYNPFIELDS
       EXCEPTIONS
            OTHERS               = 9.

  READ TABLE DYNPFIELDS WITH KEY FIELDNAME = 'IT_NEW_APP227-FORDER'.
* -- WORK ORDER SEARCH
  SELECT DISTINCT MATNR
    INTO REASON_TAB-CODE
    FROM MARA
    WHERE MTART = 'WOCL' .
    APPEND REASON_TAB.
  ENDSELECT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD    = 'CODE'
            DYNPPROG    = SY-CPROG
            DYNPNR      = SY-DYNNR
*            dynprofield = 'P_WORDER'
            VALUE_ORG   = 'S'
       TABLES
            VALUE_TAB   = REASON_TAB.

  IF SY-SUBRC  <> 0.
    MESSAGE I000 WITH 'NOT FOUND....'.
  ENDIF.

ENDMODULE.                 " wo_c_search_help  INPUT
*&---------------------------------------------------------------------*
*&      Module  back_1210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BACK_1210 INPUT.
  SET SCREEN 1209.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " back_1210  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit_2107  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
MODULE EXIT_2107 INPUT.
  LEAVE  TO  SCREEN 0.
ENDMODULE.                 " exit_2107  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2107  INPUT
*&---------------------------------------------------------------------*
*       Searching Data
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2107 INPUT.
  CASE OK_CODE.
    WHEN 'SEA'.
      PERFORM SEARCH_DATA_APP237 USING WA_MODEL
                                       ST_APP237-INQS
                                       ST_APP237-DDAY
                                       ST_APP237-BODYNO .
    WHEN 'DAILY'.
      PERFORM DISPLAY_DAILY_DELAY_CAR.
    WHEN 'EXCL'.
      PERFORM DOWNLOAD_APP237.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2107  INPUT

*&---------------------------------------------------------------------*
*&      Module  exit_100  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit command.
*----------------------------------------------------------------------*
MODULE EXIT_1203 INPUT.
  CHECK  SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
  IF WA_EDIT  NE 'X'.
    LEAVE  TO  SCREEN  0.
  ENDIF.

  PERFORM PROCESS_CONFIRM_1203 .
ENDMODULE.                 " exit_100  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1203  INPUT
*&---------------------------------------------------------------------*
*       Setting System's Commands
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1203 INPUT.
  CASE  OK_CODE.
    WHEN  'ENTER'.
*     Read Nation Information
      PERFORM CLEAR_STRUCTURE_APP221.
      PERFORM  NATION_DISPLAY_1203.
    WHEN  'PREVN'.
*     Display The Previous Nation Information
      PERFORM  PREV_NATION_DISPLAY_1203.
    WHEN  'NEXTN'.
*     Display The Next Nation Information
      PERFORM  NEXT_NATION_DISPLAY_1203.
    WHEN  'SAVE'.          "Change data save
      PERFORM  NATION_DATA_SAVE_1203.
      CLEAR: WA_CHANGE, WA_EDIT, WA_INSERT.
    WHEN  'ISRT'.
      WA_INSERT = WA_CHANGE = 'X' .
      PERFORM  NATION_DATA_INSERT_1203.
    WHEN  'TRANS'.
*     Transport Data To Legacy Sys.
      PERFORM  TRANSPORT_TO_ALC_1203.
*    when  'BACK' or 'EXIT'.
*      perform clear_structure_app221.
*      perform exit_process_1203.
    WHEN 'CS'.
      PERFORM CLEAR_STRUCTURE_APP221.
      PERFORM  NATION_DISPLAY_1203.
    WHEN 'SCHNG'.
      IF WA_CHANGE = SPACE.
        WA_CHANGE = 'X' .
      ELSE.
        WA_CHANGE = ' ' .
      ENDIF.
  ENDCASE.
ENDMODULE.                 " user_command_1203  INPUT

*&---------------------------------------------------------------------*
*&      Module  drive_type  INPUT
*&---------------------------------------------------------------------*
*       Handling Error - Drive Type
*----------------------------------------------------------------------*
MODULE DRIVE_TYPE_1203 INPUT.
  WA_EDIT = 'X' .
  IF IS_APP221-DRIVE  EQ  'L' OR IS_APP221-DRIVE  EQ 'R'.
  ELSE.
    MESSAGE E000  WITH  'Drive Type is Mismatch'.
  ENDIF.
ENDMODULE.                 " drive_type  INPUT

*&---------------------------------------------------------------------*
*&      Module  search_data_app219  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_DATA_APP219 INPUT.
  PERFORM SEARCH_DATA_APP219 .
ENDMODULE.                 " search_data_app219  INPUT

*&---------------------------------------------------------------------*
*&      Module  exit_100  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
MODULE EXIT_100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit_100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands For Main Screen
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2106 INPUT.
  CASE OK_CODE.
    WHEN 'ATTR'.
      PERFORM  KEY_FIELD_ATTR_APP236.
      CLEAR: SY-UCOMM. EXIT.
    WHEN 'SEA'.
*     CASE st_key_app236-inqopt.
*       WHEN 'VEH'.
      PERFORM  VEHICLE_NUMBER_SEARCH_APP236.
*       WHEN 'VIN'.
*         PERFORM  vin_number_search_app236.
*       WHEN 'ENG'.
*         PERFORM  engine_number_search_app236.
*       WHEN 'TMN'.
*         PERFORM  tm_number_search_app236.
*     ENDCASE.
    WHEN 'NEXD'  .
      PERFORM GET_NUMBER USING '+' .
      PERFORM  VEHICLE_NUMBER_SEARCH_APP236.
    WHEN 'PRED'  .
      PERFORM GET_NUMBER USING '-' .
      PERFORM  VEHICLE_NUMBER_SEARCH_APP236.
    WHEN 'EXCEL'.
      PERFORM  EXPORT_TO_EXCEL.
    WHEN '219OPT'.
      PERFORM  219_OPTION_DISPLAY.   "popup
    WHEN 'AIRBAG'.
      PERFORM  AIRBAG_DISPLAY.
    WHEN 'ORDER'.
      PERFORM  ORDER_LIST_DISPLAY.   "popup
*    WHEN 'CHASSI'.
*    WHEN 'APPR'.
*    WHEN 'CAP'.
  ENDCASE.
  CLEAR: SY-UCOMM.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  field_bodyno_check  INPUT
*&---------------------------------------------------------------------*
*       Checking Vehicle Master No.
*----------------------------------------------------------------------*
MODULE FIELD_BODYNO_CHECK_APP236 INPUT.
  CHECK  NOT ST_APP236-MODEL  IS INITIAL AND
         NOT ST_APP236-BODYNO IS INITIAL.

  CONCATENATE  WA_MODEL  ST_APP236-BODYNO  INTO  G_EQUNR_APP236.

  PERFORM  EQUI_MASTER_CHECK_APP236  USING  G_EQUNR_APP236  .
ENDMODULE.                 " field_bodyno_check  INPUT

*&---------------------------------------------------------------------*
*&      Module  field_vin_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE FIELD_VIN_CHECK_APP236 INPUT.
  CLEAR: ST_APP236-BODYNO.
ENDMODULE.                 " field_vin_check  INPUT

*&---------------------------------------------------------------------*
*&      Module  field_engine_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE FIELD_ENGINE_CHECK_APP236 INPUT.

ENDMODULE.                 " field_engine_check  INPUT

*&---------------------------------------------------------------------*
*&      Module  field_tm_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE FIELD_TM_CHECK_APP236 INPUT.

ENDMODULE.                 " field_tm_check  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command - 219 Option List
*----------------------------------------------------------------------*
*module user_command_0110 input.
*  if sy-ucomm eq 'EXIT'.
*    leave to screen  0.
*  endif.
*endmodule.                 " USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands - Order List
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0120 INPUT.

  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN  0.
    WHEN 'UPART'.
      PERFORM  UNIQUE_PART_DISPLAY.
    WHEN 'CPART'.
      PERFORM  COLOR_PART_DISPLAY.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0130  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command - Air Bag List
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0130 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0130  INPUT

* INPUT MODULE FOR TABSTRIP 'SS2106': GETS ACTIVE TAB
MODULE SS2106_ACTIVE_TAB_GET INPUT.
  OK_CODE = SY-UCOMM.
  CASE OK_CODE.
    WHEN C_SS2106-TAB1.
      G_SS2106-PRESSED_TAB = C_SS2106-TAB1.
    WHEN C_SS2106-TAB2.
      G_SS2106-PRESSED_TAB = C_SS2106-TAB2.
    WHEN C_SS2106-TAB3.
      G_SS2106-PRESSED_TAB = C_SS2106-TAB3.
    WHEN C_SS2106-TAB4.
      G_SS2106-PRESSED_TAB = C_SS2106-TAB4.
    WHEN C_SS2106-TAB5.
      G_SS2106-PRESSED_TAB = C_SS2106-TAB5.
    WHEN C_SS2106-TAB6.
      G_SS2106-PRESSED_TAB = C_SS2106-TAB6.
    WHEN C_SS2106-TAB7.
      G_SS2106-PRESSED_TAB = C_SS2106-TAB7.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.

ENDMODULE.

**&---------------------------------------------------------------------
*
**&      Module  modify_data_APP246  INPUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
*MODULE modify_data_APP246 INPUT.
*  MODIFY IT_APP246 INDEX TC_APP246-current_line.
*
*ENDMODULE.                 " modify_data_APP246  INPUT
**&---------------------------------------------------------------------
*
**&      Module  user_command_APP246  INPUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
MODULE USER_COMMAND_APP246 INPUT.
  CASE OK_CODE.
    WHEN OTHERS.
      IF NOT ( GS_CUSTOM_CONTAINER IS INITIAL ).
        CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
        FREE  GS_CUSTOM_CONTAINER.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " user_command_APP246  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE exit INPUT.
*  LEAVE PROGRAM.
*ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  read_data_app246  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_DATA_APP246 INPUT.
  IF NOT ( GS_CUSTOM_CONTAINER IS INITIAL ).
    CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
    FREE  GS_CUSTOM_CONTAINER.
  ENDIF.
  PERFORM MAKE_PROGRESS_RANGE.                              "UD1K912914
  PERFORM CHECK_AND_READ_DATA_APP246 .
ENDMODULE.                 " read_data_app246  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_cursor_field_app246  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD_APP246 INPUT.
  CLEAR: WA_FNAME_TX, WA_SAVELINE_IX.
  GET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " get_cursor_field_app246  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_data_APP245  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_APP245 INPUT.
  MODIFY IT_APP245 INDEX TC_APP245-CURRENT_LINE.

ENDMODULE.                 " modify_data_APP245  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_APP245  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_APP245 INPUT.
  CASE OK_CODE.
    WHEN 'SEA'.
      CLEAR OK_CODE.
      PERFORM SEARCH_DATA_APP245.
    WHEN 'EXC'.
      CLEAR OK_CODE.
      PERFORM DOWNLOAD_DATA_APP245.
    WHEN 'SORTA'. "SORTING ASCENDING.
      CLEAR OK_CODE.
*     Sort By Ascending
      PERFORM SORT_SCREEN_2116     USING 'A'.
    WHEN 'SORTD'. "SORTING DESCENDING.
      CLEAR OK_CODE.
*     Sort By Descending
      PERFORM SORT_SCREEN_2116     USING 'D'.
  ENDCASE.
ENDMODULE.                 " user_command_APP245  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_data_APP244  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_APP244 INPUT.
  MODIFY IT_APP244 INDEX TC_APP244-CURRENT_LINE.

ENDMODULE.                 " modify_data_APP244  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_APP244  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_APP244 INPUT.
  CASE OK_CODE.
    WHEN 'SEA'.
      CLEAR OK_CODE.
      PERFORM SEARCH_DATA_APP244.
    WHEN 'EXC'.
      CLEAR OK_CODE.
      PERFORM DOWNLOAD_DATA_APP244.
    WHEN 'SORTA'. "SORTING ASCENDING.
      CLEAR OK_CODE.
*     Sort By Ascending
      PERFORM SORT_SCREEN_2115  USING 'A'.
    WHEN 'SORTD'. "SORTING DESCENDING.
      CLEAR OK_CODE.
*     Sort By Descending
      PERFORM SORT_SCREEN_2115  USING 'D'.
    WHEN 'PICK'.
      PERFORM CALL_TCODE.
  ENDCASE.
ENDMODULE.                 " user_command_APP244  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_data_app240  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_APP240 INPUT.
  MODIFY IT_APP240 INDEX TC_APP240-CURRENT_LINE.

ENDMODULE.                 " modify_data_app240  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_app240  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_APP240 INPUT.
  CASE OK_CODE.
    WHEN 'SEA'.
      CLEAR OK_CODE.
      PERFORM SEARCH_DATA_APP240.
    WHEN 'EXC'.
      CLEAR OK_CODE.
      PERFORM DOWNLOAD_DATA_APP240.
    WHEN 'SORTA'. "SORTING ASCENDING.
      CLEAR OK_CODE.
*     Sort By Ascending
      PERFORM SORT_SCREEN_2114   USING 'A'.
    WHEN 'SORTD'. "SORTING DESCENDING.
      CLEAR OK_CODE.
*     Sort By Descending
      PERFORM SORT_SCREEN_2114   USING 'D'.
    WHEN 'BACK' OR 'EXIT'.
      EXIT.
  ENDCASE.

  DESCRIBE TABLE IT_APP240 LINES WA_LINES.
  IF WA_LINES = 0.
    MESSAGE W001 WITH TEXT-100.
  ENDIF.
ENDMODULE.                 " user_command_app240  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3107  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_3107 INPUT.
* There should be more discussions about functions.
  CASE OK_CODE.
    WHEN 'ZDIS'.  "DISPLAY
      PERFORM DATA_SELECT_3107 .
    WHEN 'ZEXCL'.
      PERFORM EXCEL_DOWN_3107 .
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
      PERFORM COMPUTE_SCROLLING_IN_TC USING 'TC_3107'  OK_CODE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_3107  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*    Modification of Internal Table with Table Control's Current line
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_APP239 INPUT.
  MODIFY IT_APP239 INDEX TC_APP239-CURRENT_LINE.
ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0110  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
MODULE USER_COMMAND_APP239 INPUT.
  CASE OK_CODE.
    WHEN 'SEL'.
      PERFORM MAKE_DATA_APP239.
    WHEN 'EXL'.
      PERFORM DOWNLOAD_APP239.
    WHEN 'SORTA'. "SORTING ASCENDING.
      PERFORM SORT_SCREEN_2113    USING 'A'.
    WHEN 'SORTD'. "SORTING DESCENDING.
      PERFORM SORT_SCREEN_2113    USING 'D'.
  ENDCASE.

  DESCRIBE TABLE IT_APP239 LINES WA_LINES.
  IF WA_LINES = 0.
    MESSAGE S001 WITH TEXT-100.
  ENDIF.
ENDMODULE.                 " user_command_0110  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_APP272 INPUT.
  CASE OK_CODE .
    WHEN '/CS'.
      PERFORM READ_PROCESS_APP272.
      DESCRIBE TABLE IT_APP272_01 LINES TC_APP272_01-LINES.
** Changed by Furong on 03/30/07  >> Help desk: 73V9584258
**                                >> Transport Request: UD1K940212
    WHEN 'EXC'.
      CLEAR OK_CODE.
      PERFORM DOWNLOAD_DATA_APP272.
  ENDCASE.
** end of change
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  DEFINE_DISPLAY_app301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DEFINE_DISPLAY_APP301 INPUT.
  CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
  FREE  GS_CUSTOM_CONTAINER.
  CLEAR: IT_HOUR_APP301[], IT_DAY_APP301[], IT_WEEK_APP301[].
ENDMODULE.                 " DEFINE_DISPLAY_app301  INPUT

*&---------------------------------------------------------------------*
*&      Module  get_cursor_field_app301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD_APP301 INPUT.
  CLEAR: WA_FNAME_TX, WA_SAVELINE_IX.
  GET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " get_cursor_field_app301  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_app301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_APP301 INPUT.
  CASE OK_CODE.
    WHEN 'SEA'.
      PERFORM SEARCH_DATA_APP301.
    WHEN OTHERS.
      IF NOT ( GS_CUSTOM_CONTAINER IS INITIAL ).
        CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
        FREE  GS_CUSTOM_CONTAINER.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " user_command_app301  INPUT
*&---------------------------------------------------------------------*
*&      Module  read_text_inf  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_TEXT_INF_APP301 INPUT.
  DATA: L_NUM(03) TYPE N.

  MOVE P_COLUMN_APP301 TO L_NUM.

  SELECT SINGLE CLNM
    INTO P_COL_NAME_APP301
    FROM ZTBM_ABXOPVDT
    WHERE CLNO = L_NUM.

  PERFORM READ_TABLE_TEXT  USING WA_MODEL         P_PART_APP301
                                 P_COLUMN_APP301  P_COL_NAME_APP301.
ENDMODULE.                 " read_text_inf  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_APP302  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_APP302 INPUT.
  CASE OK_CODE.
    WHEN 'SEA'.
      PERFORM SEARCH_DATA_APP302.
    WHEN 'ACD'.
      PERFORM SORT_SCREEN_4102  USING 'A'.
    WHEN 'DEC'.
      PERFORM SORT_SCREEN_4102  USING 'D'.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " user_command_APP302  INPUT

*&---------------------------------------------------------------------*
*&      Module  read_text_inf  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_TEXT_INF_APP302 INPUT.
*  DATA: l_num(03) TYPE n.
  MOVE P_COLUMN_APP302 TO L_NUM.
  SELECT SINGLE VANM
    INTO P_COL_NAME_APP302
    FROM ZTBM_ABXOPVDT
    WHERE CLNO = L_NUM.
  PERFORM READ_TABLE_TEXT  USING WA_MODEL         P_PART_APP302
                                 P_COLUMN_APP302  P_COL_NAME_APP302.
ENDMODULE.                 " read_text_inf  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_3109  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_3109 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      PERFORM DATA_SELECT_3109 .
    WHEN 'ZEXCL'.
  ENDCASE.
ENDMODULE.                 " user_command_3109  INPUT

*&---------------------------------------------------------------------*
*&      Module  model_desc_find_app220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODEL_DESC_FIND_APP220 INPUT.
  PERFORM 219_COLUMN_VALUE.
  PERFORM  COLUMN_VALUE_SELECT.
ENDMODULE.                 " model_desc_find_app220  INPUT
*&---------------------------------------------------------------------*
*&      Module  column_value_find_app220  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COLUMN_VALUE_FIND_APP220 INPUT.
  PERFORM 219_COLUMN_VALUE.
  PERFORM  COLUMN_VALUE_SELECT.
ENDMODULE.                 " column_value_find_app220  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1202 INPUT.
  CASE SY-UCOMM.
    WHEN  'PCOL'.
      PERFORM CLEAR_INT_TABLE_APP220.
      PERFORM  PREV_COLUMN_SELECT.
    WHEN  'NCOL'.
      PERFORM CLEAR_INT_TABLE_APP220.
      PERFORM  NEXT_COLUMN_SELECT.
    WHEN  'ENTER'.
      PERFORM CLEAR_INT_TABLE_APP220.
      PERFORM  COLUMN_VALUE_SELECT.
    WHEN  'CS'.
      PERFORM CLEAR_INT_TABLE_APP220.
      PERFORM  COLUMN_VALUE_SELECT.
  ENDCASE.

ENDMODULE.                 " user_command_1202  INPUT
*&---------------------------------------------------------------------*
*&      Module  4104_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 4104_PAI INPUT.

ENDMODULE.                 " 4104_pai  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_4104 INPUT.
  CASE OK_CODE.
    WHEN 'PRNT'.
      " Call the Parameters for the Print....
*     PERFORM WRITE_DATA      .
    WHEN 'RUN' OR 'WSC'.
      PERFORM CALL_TRANS_4104 .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_4104  INPUT

*&---------------------------------------------------------------------*
*&      Module  set_comm  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_COMM INPUT.
  OK_CODE = 'ATTR'.
ENDMODULE.                 " set_comm  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data_2301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_DATA_2301 INPUT.

ENDMODULE.                 " modify_data_2301  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_2301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2301 INPUT.
  CASE OK_CODE.
    WHEN 'SEA'.
      PERFORM SEARCH_DATA_APP263 .
  ENDCASE.
ENDMODULE.                 " user_command_2301  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_inernal_table_app207  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_INTERNAL_TABLE_APP207 INPUT.
  MODIFY IT_ERR INDEX TC_A107-CURRENT_LINE.
ENDMODULE.                 " modify_inernal_table_app207  INPUT

*&---------------------------------------------------------------------*
*&      Module  search_wo_14  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_WO_14 INPUT.

ENDMODULE.                 " search_wo_14  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.
*  GET PARAMETER ID 'ZWORKN' FIELD it_0118-order.
*  IF NOT it_0118-order IS INITIAL.
*   R02 = 'X'.
*  ENDIF.
**  FREE MEMORY WORKN.

  CASE OK_CODE.
    WHEN 'PLAN'.
      SET PARAMETER ID 'ZRUN' FIELD 'X' .
      CALL TRANSACTION 'ZPPR0903' .
      SET PARAMETER ID 'ZRUN' FIELD ' ' .

** Furong on 12/02/11
*    WHEN 'SCR' .
    WHEN 'SCR' OR 'PICK'.
** End on 12/02/11
      CASE 'X'.
        WHEN R01.   OK_CODE = '0101'.
        WHEN R02.   OK_CODE = '0102'.
        WHEN R03.   OK_CODE = '0103'.
        WHEN R04.   OK_CODE = '0104'.
        WHEN R05.   OK_CODE = '0105'.
        WHEN R06.   OK_CODE = '0106'.
        WHEN R07.   OK_CODE = '0107'.
        WHEN R08.   OK_CODE = '0109'.
        WHEN R81.   OK_CODE = '8081'.
        WHEN R82.   OK_CODE = '8082'.
        WHEN R09.   OK_CODE = '0110'.
        WHEN R10.   OK_CODE = '0111'.
        WHEN R11.   OK_CODE = '0118'.
        WHEN R12.   OK_CODE = '1201'.
        WHEN R13.   OK_CODE = '1202'.
        WHEN R14.   OK_CODE = '1203'.
        WHEN R15.   OK_CODE = '1205'.
        WHEN R16.   OK_CODE = '2101'.
        WHEN R17.   OK_CODE = '2102'.
        WHEN R18.   OK_CODE = '2103'.
        WHEN R19.   OK_CODE = '2106'.
        WHEN R20.   OK_CODE = '2107'.
        WHEN R21.   OK_CODE = '2113'.
        WHEN R22.   OK_CODE = '2114'.
        WHEN R23.   OK_CODE = '2117'.
        WHEN R24.   OK_CODE = '2115'.
        WHEN R25.   OK_CODE = '2116'.
        WHEN R26.   OK_CODE = '2200'.
        WHEN R27.   OK_CODE = '2201'.
        WHEN R28.   OK_CODE = '2202'.
        WHEN R29.   OK_CODE = '2203'.
        WHEN R30.   OK_CODE = '2204'.
        WHEN R31.   OK_CODE = '2205'.
        WHEN R32.   OK_CODE = '2206'.
        WHEN R33.   OK_CODE = '2301'.
        WHEN R34.   OK_CODE = '3104'.
        WHEN R35.   OK_CODE = '3107'.
        WHEN R36.   OK_CODE = '3109'.
        WHEN R37.   OK_CODE = '3211'.
        WHEN R38.   OK_CODE = '3301'.
        WHEN R39.   OK_CODE = '4101'.
        WHEN R40.   OK_CODE = '4102'.
        WHEN R41.   OK_CODE = '4103'.
        WHEN R42. CALL TRANSACTION 'ZPPR0205' . "AND SKIP FIRST SCREEN .
        WHEN R43. CALL TRANSACTION 'ZPPR0207' . "AND SKIP FIRST SCREEN .
        WHEN R44. CALL TRANSACTION 'ZPPR0208' . "AND SKIP FIRST SCREEN .
        WHEN R45. CALL TRANSACTION 'ZPPR0204' . "AND SKIP FIRST SCREEN .
        WHEN R46.   OK_CODE = '0101'.
        WHEN R47.   OK_CODE = '0101'.
        WHEN R48.   OK_CODE = '0101'.
        WHEN R49.   OK_CODE = '0101'.
        WHEN R50.   OK_CODE = '0101'.
        WHEN R51. CALL TRANSACTION 'ZPPR301'.
        WHEN R99.   OK_CODE = '4104'.
        WHEN R60.  CALL TRANSACTION 'ZPPR803_PLAN_FSC'.
        WHEN R88.   OK_CODE = '8088'.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_TC2202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_TC2202 INPUT.
  MODIFY IT_2202 INDEX TC_2202-CURRENT_LINE.
ENDMODULE.                 " MODIFY_TC2202  INPUT

*&---------------------------------------------------------------------*
*&      Module  TEXT_DESCRIPTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TEXT_DESCRIPTION INPUT.
  DATA: L_TYPE          TYPE C,
        L_CHAR(30)      TYPE C.

  IF ST_3107_INPUT-COLA NE SPACE .
    L_TYPE = ST_3107_INPUT-TABLEA+6(1) .
    PERFORM READ_TABLE_TEXT  USING WA_MODEL             L_TYPE
                             ST_3107_INPUT-COLA  ST_3107_INPUT-TEXTA.
  ENDIF.

  IF ST_3107_INPUT-COLB NE SPACE .
    CONCATENATE ST_3107_INPUT-TABLEB ST_3107_INPUT-COLB INTO L_CHAR.
    SELECT SINGLE ATBEZ INTO ST_3107_INPUT-TEXTB
      FROM CABN AS C INNER JOIN CABNT AS T
        ON C~ATINN = T~ATINN
     WHERE C~ATNAM = L_CHAR
       AND T~SPRAS = SY-LANGU .
  ENDIF.
ENDMODULE.                 " TEXT_DESCRIPTION  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_UPH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_UPH INPUT .
  PERFORM SET_UPH  .
*     USING st_5290_input-uph_l st_5290_input-uph_h    "UD1K912931
*           'T'                 st_5290_input-date     "UD1K912931
*           st_5290_input-dayu  st_5290_input-day      "UD1K912931
*           st_5290_input-ftime st_5290_input-ttime  . "UD1K912931
ENDMODULE.                 " SET_UPH  INPUT

*&---------------------------------------------------------------------*
*&      Module  CLEAR_0107  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CLEAR_0107 INPUT.
  CLEAR: WA_CAR,
         WA_MODEL,
         WA_MI,
         WA_OCN,
         WA_TRIM_PLANT_NO,
         WA_WOC_DATE,
         WA_WOM_DATE,
         WA_LC_NO,
         WA_DESTINATION_CODE,
         WA_PERF_YN,
         WA_VIN_SPEC,
         WA_INIT_QTY,
         WA_MOD_QTY,
         IT_219,
         IT_219[].
ENDMODULE.                 " CLEAR_0107  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_FLAG_1203  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_FLAG_1203 INPUT.
  WA_EDIT = 'X' .
ENDMODULE.                 " SET_FLAG_1203  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_3211  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_3211 INPUT.
  CHECK OK_CODE = 'ZDIS'.
  IF ST_4279_INPUT-DURA = 0.
    MESSAGE E001 WITH TEXT-003 .
  ENDIF.
ENDMODULE.                 " CHECK_3211  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_2108  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_2108 OUTPUT.
*  SET PF-STATUS 'STATUS2108'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_2108  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_DATA OUTPUT.
  PERFORM GET_DATA_2108.
  PERFORM CREAT_ALV_2108.
*  PERFORM DISPLAY_DATA_2108.
ENDMODULE.                 " GET_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  field_wono_cursor_app236  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE FIELD_WONO_CURSOR_APP236 INPUT.
  DATA: W_FIELD(20),
        WA_EQU LIKE RM63E-EQUNR.
  IF OK_CODE = 'PICK'.
    GET CURSOR FIELD W_FIELD.
    CASE W_FIELD.
      WHEN 'ST_APP236-PLNUM'.
        SET PARAMETER ID 'PAF' FIELD ST_APP236-PLNUM.
        CALL TRANSACTION 'MD13' AND SKIP FIRST SCREEN.
      WHEN 'ST_APP236-BODYNO'.
        CONCATENATE WA_MODEL ST_APP236-BODYNO INTO WA_EQU.
        SET PARAMETER ID 'EQN' FIELD WA_EQU.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.
      WHEN 'ST_APP236-VBELN'.
        SET PARAMETER ID 'AUN' FIELD ST_APP236-VBELN.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      WHEN 'ST_APP236-ENGNO'.
        SET PARAMETER ID 'EQN' FIELD ST_APP236-ENGNO.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.

    ENDCASE.
  ENDIF.
ENDMODULE.                 " field_wono_cursor_app236  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_8081  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_8081 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      IF ST_8081_INPUT-MONTY = ' '.
        MESSAGE I001 WITH 'Please make selection of monthly pack'.
        EXIT.
      ENDIF.
      PERFORM DATA_SELECT_8081.
      DESCRIBE TABLE IT_8081 LINES WA_LINES .
      IF WA_LINES = 0.
        MESSAGE S001 WITH TEXT-100.
      ENDIF.
    WHEN 'ZEXCL'.
      PERFORM EXCEL_DOWN_8081.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
*        PERFORM compute_scrolling_in_tc USING 'TC_8081'
*                                                        ok_code.
      PERFORM PAGING USING OK_CODE.
  ENDCASE.
ENDMODULE.                 " user_command_8081  INPUT
*&---------------------------------------------------------------------*
*&      Module  entry_check_8081  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ENTRY_CHECK_8081 INPUT.
  IF ST_8081_INPUT-MONTY = ' '.
    MESSAGE I001 WITH 'Please make selection of monthly pack'.
  ENDIF.

*  IF ST_8081_INPUT-COLOR = ' '.
*    MESSAGE I001 WITH 'Please make selection for color'.
*  ENDIF.
ENDMODULE.                 " entry_check_8081  INPUT
*&---------------------------------------------------------------------*
*&      Module  set_line_count  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_LINE_COUNT INPUT.
  LINE_COUNT = SY-LOOPC.
ENDMODULE.                 " set_line_count  INPUT
*&---------------------------------------------------------------------*
*&      Module  entry_check_8082  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ENTRY_CHECK_8082 INPUT.
  DATA: L_FYYMM(6) TYPE N,
        L_TYYMM(6) TYPE N.

  IF ST_8082_INPUT-FYEAR = ' ' OR ST_8082_INPUT-FMONTH = ' '.
    MESSAGE I001 WITH 'Please make selection of year and month'.
  ELSEIF ST_8082_INPUT-TMONTH = ' '.
*       st_8082_input-tyear = st_8082_input-fyear.
    ST_8082_INPUT-TMONTH = ST_8082_INPUT-FMONTH.
  ELSE.
    CONCATENATE ST_8082_INPUT-FYEAR ST_8082_INPUT-FMONTH INTO L_FYYMM.
    CONCATENATE ST_8082_INPUT-FYEAR ST_8082_INPUT-TMONTH INTO L_TYYMM.
    IF L_TYYMM < L_FYYMM.
        MESSAGE I001 WITH
        'To_year & To_month cannot be earlier than FR_YEAR & FR_MONTH'.
    ENDIF.
  ENDIF.
ENDMODULE.                 " entry_check_8082  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_8082  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_8082 INPUT.
  CASE OK_CODE.
    WHEN 'ZDIS'.
      IF ST_8082_INPUT-FYEAR = ' ' OR
         ST_8082_INPUT-FMONTH = ' '.
        MESSAGE I001 WITH 'Please make selection of Year and Month'.
        EXIT.
      ENDIF.
      PERFORM DATA_SELECT_8082.
      DESCRIBE TABLE IT_8082 LINES WA_LINES .
      IF WA_LINES = 0.
        MESSAGE S001 WITH TEXT-100.
      ENDIF.
    WHEN 'ZEXCL'.
      PERFORM EXCEL_DOWN_8082.
    WHEN 'P+' OR 'P++' OR 'P-' OR 'P--'.
*        PERFORM compute_scrolling_in_tc USING 'TC_8081'
*                                                        ok_code.
      PERFORM PAGE USING OK_CODE TB_8082.
  ENDCASE.

ENDMODULE.                 " user_command_8082  INPUT
*&---------------------------------------------------------------------*
*&      Module  8088_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 8088_PAI INPUT.

ENDMODULE.                 " 8088_pai  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8088  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_8088 INPUT.
  CASE OK_CODE.
    WHEN 'RUN' OR 'PDT'.
      PERFORM CALL_TRANS_8088.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_8088  INPUT
