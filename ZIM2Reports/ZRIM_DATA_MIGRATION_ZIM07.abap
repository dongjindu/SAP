
REPORT ZRIM_DATA_MIGRATION_ZIM07 NO STANDARD PAGE HEADING LINE-SIZE 255.

INCLUDE ZRIM_DATA_MIG_F01.
TABLES : EKKO.
DATA: BEGIN OF IT_ZTREQHD OCCURS 0,
        EBELN(010),
*        ZFMATGB(001),
*        ZFTRANS(001),
*        ZFREQSD(010),
*        ZFREQED(010),
*        ZFSHCU(003),
*        ZFSPRTC(003),
*        ZFARCU(003),
*        ZFAPRTC(003),
*        ZFOPBN(010),
     END OF IT_ZTREQHD.

*==============================================
PARAMETERS : P_PATH LIKE RLGRAP-FILENAME.
PARAMETERS : P_MODE TYPE C.
*===============================================
START-OF-SELECTION.

  PERFORM READ_DATA_FILE TABLES IT_ZTREQHD
                         USING  P_PATH.

  LOOP AT IT_ZTREQHD.
    REFRESH : BDCDATA. CLEAR BDCDATA.

*>> L/C OPEN 초기화면.
    PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMZIM00'      '0700',
                  ' ' 'BDC_OKCODE'     '=ENTR',
                  ' ' 'ZSREQHD-EBELN'  IT_ZTREQHD-EBELN,
                  ' ' 'ZSREQHD-ZFREQNO' ' '.

    CLEAR EKKO.
    SELECT SINGLE * FROM EKKO
                   WHERE EBELN EQ IT_ZTREQHD-EBELN.

    IF SY-SUBRC NE 0.
      WRITE : / '구매문서번호 :', IT_ZTREQHD-EBELN,
                '가 존재하지 않습니다.'.
      CONTINUE.
    ENDIF.
*>> T/T OPEN 일반사항.
    IF EKKO-ZTERM(2) EQ 'TT'.
      PERFORM P2000_DYNPRO USING :
                   'X' 'SAPMZIM00'        '0141',
                   ' ' 'BDC_OKCODE'       '=SAVE'.

    ELSE.
*>> L/C OPEN 일반사항.
      PERFORM P2000_DYNPRO USING :
                   'X' 'SAPMZIM00' '0101',
                   ' ' 'BDC_OKCODE'       '=SAVE'.
    ENDIF.

    PERFORM P2000_DYNPRO USING :
                 ' ' 'ZTREQST-ZFOPNDT'   SY-DATUM,
                 ' ' 'ZTREQST-ZFOPNNO'   IT_ZTREQHD-EBELN.


*>> SAVE DIALOG BOX.
    PERFORM P2000_DYNPRO USING :
                    'X' 'SAPMZIM00'         '0001',
                    ' ' 'BDC_OKCODE'        '=YES'.

    PERFORM BDC_TRANSACTION USING 'ZIM07'  P_MODE
                         CHANGING  W_SUBRC.
    IF W_SUBRC NE 0.
      WRITE : '(구매문서번호 :', IT_ZTREQHD-EBELN, ')'.
    ENDIF.

  ENDLOOP.
