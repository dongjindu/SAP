FUNCTION ZIM_CHECK_NUMBER_RANGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZFREQTY) LIKE  ZTREQHD-ZFREQTY OPTIONAL
*"  CHANGING
*"     REFERENCE(W_ZTIMIMG00) LIKE  ZTIMIMG00 STRUCTURE  ZTIMIMG00
*"  EXCEPTIONS
*"      NOT_INPUT
*"      NOT_TYPE
*"      NOT_RANGE
*"      NOT_MATCH_TYPE
*"      DUPLICATE_RANGE
*"      NOT_NUMERIC
*"      RANGE_OVER
*"----------------------------------------------------------------------
  DATA : IMPORT_CHK.
  DATA : W_ZFREQTY    LIKE   ZTREQHD-ZFREQTY.
  DATA : W_DUP_TYPE   LIKE   ZTREQHD-ZFREQTY.
  DATA : W_TEMP_NO    LIKE   ZTREQHD-ZFREQNO.
  DATA : W_DATATYPE   LIKE   DD01V-DATATYPE.
  DATA : W_TEMP_TEXT(255).
  DATA : W_ERROR_CHK.

  DATA : BEGIN OF DOC_TYPE,
            LC(2)    TYPE C       VALUE 'LC',
            LO(2)    TYPE C       VALUE 'LO',
            PU(2)    TYPE C       VALUE 'PU',
            TT(2)    TYPE C       VALUE 'TT',
            DA(2)    TYPE C       VALUE 'DA',
            DP(2)    TYPE C       VALUE 'DP',
            OF(2)    TYPE C       VALUE 'OF',
            BL(2)    TYPE C       VALUE 'BL',
            IV(2)    TYPE C       VALUE 'IV',
            CI(2)    TYPE C       VALUE 'CI',
            CG(2)    TYPE C       VALUE 'CG',
            VT(2)    TYPE C       VALUE 'VT',
            MS(2)    TYPE C       VALUE 'MS',
            RE(2)    TYPE C       VALUE 'RE',
         END   OF DOC_TYPE.

  FIELD-SYMBOLS : <FS_FROM>, <FS_TO>, <FS_CURR>, <FS_NCURR>.

* CINFIG. TABLE MOVE
  ZTIMIMG00 = W_ZTIMIMG00.
*
** IMPORT PARAMETER CHECK
*  IF NOT ZFREQTY IS INITIAL.
*     IMPORT_CHK = 'X'.
*  ELSE.
*     CLEAR : IMPORT_CHK, W_ZFREQTY.
*  ENDIF.
*
** LOOP
*  DO VARYING W_ZFREQTY FROM DOC_TYPE-LC NEXT DOC_TYPE-LO.
*     IF NOT IMPORT_CHK IS INITIAL.
*        IF  ZFREQTY NE W_ZFREQTY.
*            IF W_ZFREQTY EQ 'RE'.      EXIT.   ENDIF.
*            CONTINUE.
*        ENDIF.
*     ENDIF.
*
*     CASE W_ZFREQTY.
*        WHEN 'LC'.         " L/C NO.
*           ASSIGN ZTIMIMG00-ZFRNMLC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFRNMLF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFRNMLT TO <FS_TO>.
*        WHEN 'LO'.         " LOCAL L/C NO.
*           ASSIGN ZTIMIMG00-ZFRNLLC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFRNLLF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFRNLLT TO <FS_TO>.
*        WHEN 'PU'.          " 구매승인서
*           ASSIGN ZTIMIMG00-ZFRNPUC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFRNPUF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFRNPUT TO <FS_TO>.
*        WHEN 'DA'.          " D/A 관리번호
*           ASSIGN ZTIMIMG00-ZFRNDAC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFRNDAF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFRNDAT TO <FS_TO>.
*        WHEN 'DP'.          " D/P 관리번호
*           IF IMPORT_CHK IS INITIAL.
*              CONTINUE.
*           ENDIF.
*           ASSIGN ZTIMIMG00-ZFRNDAC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFRNDAF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFRNDAT TO <FS_TO>.
*        WHEN 'TT'.                " 전신환 관리번호
*           ASSIGN ZTIMIMG00-ZFRNTTC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFRNTTF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFRNTTT TO <FS_TO>.
*        WHEN 'OF'.                 " LOCAL OFFER SHEET 관리번호
*           ASSIGN ZTIMIMG00-ZFOFFC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFOFFF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFOFFT TO <FS_TO>.
*        WHEN 'BL'.                   " B/L 관리번호
*           ASSIGN ZTIMIMG00-ZFRNBLC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFRNBLF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFRNBLT TO <FS_TO>.
*        WHEN 'CI'.                   " COMMERCIAL INVOICE 관리번호
*           ASSIGN ZTIMIMG00-ZFCIVC   TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFCIVF   TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFCIVT   TO <FS_TO>.
*        WHEN 'IV'.                   " 통관요청 관리번호
*           ASSIGN ZTIMIMG00-ZFIVC   TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFIVF   TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFIVT   TO <FS_TO>.
*        WHEN 'CG'.                   " 하역 관리번호
*           ASSIGN ZTIMIMG00-ZFCGNC   TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFCGNF   TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFCGNT   TO <FS_TO>.
*        WHEN 'VT'.                   " 세금계산서 관리번호
*           ASSIGN ZTIMIMG00-ZFVTBC TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFVTBF TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFVTBT TO <FS_TO>.
*        WHEN 'RE'.                   "  인수증 관리번호
*           ASSIGN ZTIMIMG00-ZFREDC  TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFREDF  TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFREDT  TO <FS_TO>.
*        WHEN 'MS'.                   "
*           ASSIGN ZTIMIMG00-ZFMSNC  TO <FS_CURR>.
*           ASSIGN ZTIMIMG00-ZFMSNF  TO <FS_FROM>.
*           ASSIGN ZTIMIMG00-ZFMSNT  TO <FS_TO>.
*        WHEN OTHERS.
*           MESSAGE E223 WITH W_ZFREQTY RAISING NOT_TYPE.
*     ENDCASE.
*
** NOT INPUT
*     IF <FS_FROM> IS INITIAL OR <FS_TO> IS INITIAL.
*        MESSAGE E224 WITH <FS_FROM> <FS_TO> RAISING NOT_INPUT.
*     ENDIF.
*
** RANGE CHECK
**    IF NOT ( <FS_CURR> >= <FS_FROM> AND <FS_CURR> <= <FS_TO> ).
*     IF NOT ( <FS_FROM> LT <FS_TO> ).
*        MESSAGE E225 WITH <FS_FROM> <FS_TO>  RAISING NOT_RANGE.
*     ENDIF.
*
*     DO 2 TIMES.
*        CLEAR : W_ERROR_CHK.
*        IF SY-INDEX EQ 1.
*           WRITE : <FS_FROM> TO W_TEMP_TEXT.
*        ELSE.
*           WRITE : <FS_TO> TO W_TEMP_TEXT.
*        ENDIF.
** LEN. CHECK
*        PERFORM P2000_SPACE_CUT         CHANGING W_TEMP_TEXT.
*        PERFORM  P2000_STRING_LEN_CHECK USING    W_TEMP_TEXT
*                                                 10
*                                        CHANGING W_ERROR_CHK.
*        IF NOT W_ERROR_CHK IS INITIAL.
*           IF SY-INDEX EQ 1.
*              MESSAGE E230 WITH W_ZFREQTY 'From Range' 10.
*           ELSE.
*              MESSAGE E230 WITH W_ZFREQTY 'To Range'   10.
*           ENDIF.
*        ENDIF.
*
*        CALL FUNCTION 'NUMERIC_CHECK'
*           EXPORTING
*               STRING_IN  =   W_TEMP_TEXT
*           IMPORTING
*               STRING_OUT =   W_TEMP_TEXT
*               HTYPE      =   W_DATATYPE.
*
*        IF W_DATATYPE NE 'NUMC'.
*           IF SY-INDEX EQ 1.
*              MESSAGE E229 WITH W_ZFREQTY 'From Range'
*                             W_TEMP_TEXT(10) RAISING NOT_NUMERIC.
*           ELSE.
*              MESSAGE E229 WITH W_ZFREQTY 'To Range'
*                             W_TEMP_TEXT(10) RAISING NOT_NUMERIC.
*           ENDIF.
*        ENDIF.
*
*     ENDDO.
**
*     CASE W_ZFREQTY.
*        WHEN 'LC' OR 'LO' OR 'PU' OR 'DA' OR 'DP' OR 'TT'.  " 수입의뢰
*           CLEAR : W_DUP_TYPE.
*           PERFORM   P2000_OTHER_RANGE_CHECK
*                                 USING     W_ZFREQTY
*                                           <FS_FROM> <FS_TO>
*                                 CHANGING  ZTIMIMG00
*                                           W_DUP_TYPE.
*           IF NOT W_DUP_TYPE IS INITIAL.
*              MESSAGE E227 WITH W_ZFREQTY W_DUP_TYPE
*                           RAiSING DUPLICATE_RANGE.
*           ENDIF.
*        WHEN OTHERS.
*     ENDCASE.
*
*
** CURR. NO SELECT / CHECK
*     CASE W_ZFREQTY.
*        WHEN 'LC' OR 'LO' OR 'PU' OR 'DA' OR 'DP' OR 'TT'.  " 수입의뢰
*           SELECT MAX( ZFREQNO ) INTO    W_TEMP_NO   FROM ZTREQHD
*                  WHERE ZFREQNO  BETWEEN <FS_FROM>   AND  <FS_TO>.
*
*           ASSIGN W_TEMP_NO   TO <FS_NCURR>.
*
*           IF NOT <FS_NCURR> IS INITIAL.
*              SELECT SINGLE * FROM ZTREQHD
*                              WHERE ZFREQNO EQ <FS_NCURR>.
*              IF W_ZFREQTY NE ZTREQHD-ZFREQTY.
*                 MESSAGE E226 WITH W_ZFREQTY ZTREQHD-ZFREQTY
*                         RAISING NOT_MATCH_TYPE.
*              ENDIF.
*           ENDIF.
*        WHEN 'BL'.         " B/L
*           SELECT MAX( ZFBLNO ) INTO W_TEMP_NO  FROM ZTBL
*                  WHERE ZFBLNO BETWEEN <FS_FROM> AND <FS_TO>.
*        WHEN 'CI'.         " 통관요청
*           SELECT MAX( ZFCIVRN ) INTO W_TEMP_NO  FROM ZTCIVHD
*                  WHERE ZFCIVRN BETWEEN <FS_FROM> AND <FS_TO>.
*        WHEN 'IV'.         " INVOICE
*           SELECT MAX( ZFIVNO ) INTO W_TEMP_NO  FROM ZTIV
*                  WHERE ZFIVNO BETWEEN <FS_FROM> AND <FS_TO>.
*        WHEN 'OF'.         " LOCAL OFFER SHEET
*           SELECT MAX( ZFREQNO ) INTO W_TEMP_NO FROM ZTOFF
*                  WHERE ZFREQNO BETWEEN <FS_FROM> AND <FS_TO>.
*        WHEN 'VT'.         " 세금계산서
*           SELECT MAX( ZFVTNO )  INTO W_TEMP_NO FROM ZTVT
*                  WHERE ZFVTNO  BETWEEN <FS_FROM> AND <FS_TO>.
*        WHEN 'RE'.         " 인수증
*           SELECT MAX( ZFREDNO ) INTO W_TEMP_NO FROM ZTRED
*                  WHERE ZFREDNO BETWEEN <FS_FROM> AND <FS_TO>.
*        WHEN 'MS'.         " 모선관리.
*           SELECT MAX( ZFMSNO ) INTO W_TEMP_NO FROM ZTMSHD
*                  WHERE ZFMSNO BETWEEN <FS_FROM> AND <FS_TO>.
*        WHEN 'CG'.         " 하역관리.
*           SELECT MAX( ZFCGNO ) INTO W_TEMP_NO FROM ZTCGHD
*                  WHERE ZFCGNO BETWEEN <FS_FROM> AND <FS_TO>.
*        WHEN OTHERS.
*           MESSAGE E223 WITH W_ZFREQTY RAISING NOT_TYPE.
*     ENDCASE.
*
** CURRENCY VALUE SET
*     CASE W_ZFREQTY.
*        WHEN 'LC'.         " L/C NO.
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFRNMLC = ZTIMIMG00-ZFRNMLF.
*           ELSE.
*              ZTIMIMG00-ZFRNMLC = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFRNMLC.
*        WHEN 'LO'.         " LOCAL L/C NO.
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFRNLLC = ZTIMIMG00-ZFRNLLF.
*           ELSE.
*              ZTIMIMG00-ZFRNLLC = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFRNLLC.
*        WHEN 'PU'.          " 구매승인서
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFRNPUC = ZTIMIMG00-ZFRNPUF.
*           ELSE.
*              ZTIMIMG00-ZFRNPUC = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFRNPUC.
*        WHEN 'DA'.          " D/A 관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFRNDAC = ZTIMIMG00-ZFRNDAF.
*           ELSE.
*              ZTIMIMG00-ZFRNDAC = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFRNDAC.
*        WHEN 'DP'.          " D/P 관리번호
*           IF IMPORT_CHK IS INITIAL.
*              CONTINUE.
*           ENDIF.
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFRNDAC = ZTIMIMG00-ZFRNDAF.
*           ELSE.
*              ZTIMIMG00-ZFRNDAC = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFRNDAC.
*        WHEN 'TT'.                " 전신환 관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFRNTTC = ZTIMIMG00-ZFRNTTF.
*           ELSE.
*              ZTIMIMG00-ZFRNTTC = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFRNTTC.
*        WHEN 'OF'.                 " LOCAL OFFER SHEET 관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFOFFC = ZTIMIMG00-ZFOFFF.
*           ELSE.
*              ZTIMIMG00-ZFOFFC = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFOFFC.
*        WHEN 'BL'.                   " B/L 관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFRNBLC = ZTIMIMG00-ZFRNBLF.
*           ELSE.
*              ZTIMIMG00-ZFRNBLC = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFRNBLC.
*        WHEN 'CI'.                   " COMMERCIAL INVOICE
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFCIVC   = ZTIMIMG00-ZFCIVF.
*           ELSE.
*              ZTIMIMG00-ZFCIVC   = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFCIVC.
*        WHEN 'IV'.                   " 통관요청 관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFIVC   = ZTIMIMG00-ZFIVF.
*           ELSE.
*              ZTIMIMG00-ZFIVC   = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFIVC.
*        WHEN 'CG'.                   " 하역 관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFCGNC   = ZTIMIMG00-ZFCGNF.
*           ELSE.
*              ZTIMIMG00-ZFCGNC   = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFCGNC.
*        WHEN 'VT'.                   " 세금계산서 관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFVTBC  = ZTIMIMG00-ZFVTBF.
*           ELSE.
*              ZTIMIMG00-ZFVTBC  = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFVTBC.
*        WHEN 'RE'.                   "  인수증 관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFREDC  = ZTIMIMG00-ZFREDF.
*           ELSE.
*              ZTIMIMG00-ZFREDC  = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFREDC.
*        WHEN 'MS'.                   " 모선관리번호
*           IF W_TEMP_NO IS INITIAL.
*              ZTIMIMG00-ZFMSNC  = ZTIMIMG00-ZFMSNF.
*           ELSE.
*              ZTIMIMG00-ZFMSNC  = W_TEMP_NO.
*           ENDIF.
*           W_TEMP_NO = ZTIMIMG00-ZFMSNC.
*        WHEN OTHERS.
*           MESSAGE E223 WITH W_ZFREQTY RAISING NOT_TYPE.
*     ENDCASE.
** ASSIGN
*     ASSIGN W_TEMP_NO  TO  <FS_NCURR>.
*
*     IF NOT ( <FS_NCURR> >= <FS_FROM> AND <FS_NCURR> <= <FS_TO> ).
*        MESSAGE E228 WITH <FS_FROM> <FS_TO> <FS_NCURR>
*                          RAISING RANGE_OVER.
*     ENDIF.
*
** EXIT CHECK
*     IF NOT IMPORT_CHK IS INITIAL.
*        IF ZFREQTY EQ W_ZFREQTY.   EXIT.   ENDIF.
*     ELSE.
*        IF W_ZFREQTY EQ 'RE'.      EXIT.   ENDIF.
*     ENDIF.
*  ENDDO.
*
  W_ZTIMIMG00 = ZTIMIMG00.
*
ENDFUNCTION.
