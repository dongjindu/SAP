
REPORT ZRIM_DATA_MIGRATION_ZIM01 NO STANDARD PAGE HEADING LINE-SIZE 255.

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

*DATA: BEGIN OF IT_ZTREQIT OCCURS 0,
*        EBELN(010),
*        EBELP(005),
*        STAWN(013),
*        MENGE(017),
*      END OF IT_ZTREQIT.

DATA : W_ERROR_FLAG,
       W_REMARK(70),
       W_LINE        TYPE   I,
       W_LINE_CNT(4),
       W_STAWN(25),
       W_MENGE(25).

*==============================================
PARAMETERS : P_PATH LIKE RLGRAP-FILENAME.
PARAMETERS : P_MODE TYPE C.
*===============================================
START-OF-SELECTION.

  PERFORM READ_DATA_FILE TABLES IT_ZTREQHD
                         USING  P_PATH.

  LOOP AT IT_ZTREQHD.
    REFRESH : BDCDATA. CLEAR BDCDATA.
*>> L/C 생성 초기화면.
    PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMZIM00'      '0100',
                  ' ' 'BDC_OKCODE'     '=ENTR',
                  ' ' 'ZSREQHD-EBELN'  IT_ZTREQHD-EBELN.
    CLEAR EKKO.
    SELECT SINGLE * FROM EKKO
                   WHERE EBELN EQ IT_ZTREQHD-EBELN.

    IF SY-SUBRC NE 0.
       WRITE : / '구매문서번호 :', IT_ZTREQHD-EBELN,
                 '가 존재하지 않습니다.'.
       CONTINUE.
    ENDIF.

    IF EKKO-ZTERM(2) EQ 'TT'.
    PERFORM P2000_DYNPRO USING :
                 'X' 'SAPMZIM00'        '0141',
                 ' ' 'BDC_OKCODE'       '=SAVE'.

    ELSE.
*>> L/C 생성 일반사항.
    PERFORM P2000_DYNPRO USING :
                 'X' 'SAPMZIM00' '0101',
                 ' ' 'BDC_OKCODE'       '=SAVE'.
    ENDIF.

    PERFORM P2000_DYNPRO USING :
*                 ' ' 'ZTREQHD-ZFMATGB'  '3',     " IT_ZTREQHD-ZFMATGB,
                 ' ' 'ZTREQHD-ZFTRANS'  'O',     " IT_ZTREQHD-ZFTRANS,
*                 ' ' 'ZTREQHD-ZFREQSD'          " IT_ZTREQHD-ZFREQSD,
                 ' ' 'ZTREQHD-ZFREQED'  SY-DATUM, " IT_ZTREQHD-ZFREQED,
                 ' ' 'ZTREQHD-ZFSHCU'   'US',  " IT_ZTREQHD-ZFSHCU,
                 ' ' 'ZTREQHD-ZFSPRTC'  'BOS', " IT_ZTREQHD-ZFSPRTC,
*                 ' ' 'ZTREQHD-ZFARCU'  " IT_ZTREQHD-ZFARCU,
                 ' ' 'ZTREQHD-ZFAPRTC'  'PUS'. " IT_ZTREQHD-ZFAPRTC,
*                 ' ' 'ZTREQHD-ZFOPBN'  " IT_ZTREQHD-ZFOPBN.

*>> SAVE DIALOG BOX.
    PERFORM P2000_DYNPRO USING :
                    'X' 'SAPMZIM00'         '0001',
                    ' ' 'BDC_OKCODE'        '=YES'.

    PERFORM BDC_TRANSACTION USING 'ZIM01' P_MODE
                         CHANGING  W_SUBRC.
    IF W_SUBRC NE 0.
       WRITE : '(구매문서번호 :', IT_ZTREQHD-EBELN, ')'.
    ENDIF.


  ENDLOOP.
