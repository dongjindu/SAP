FUNCTION YTEST_Z_LABEL_002.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(P_DEST) LIKE  PRI_PARAMS-PDEST DEFAULT 'ZEBR'
*"     REFERENCE(P_ZAPPL) TYPE  YAPPL_TEST
*"     REFERENCE(P_VAR1) LIKE  YTEST_ZLAB02-Z_TXT OPTIONAL
*"     REFERENCE(P_VAR2) LIKE  YTEST_ZLAB02-Z_TXT OPTIONAL
*"     REFERENCE(P_VAR3) LIKE  YTEST_ZLAB02-Z_TXT OPTIONAL
*"     REFERENCE(P_VAR4) LIKE  YTEST_ZLAB02-Z_TXT OPTIONAL
*"     REFERENCE(P_VAR5) LIKE  YTEST_ZLAB02-Z_TXT OPTIONAL
*"  EXCEPTIONS
*"      NOT_FOUND
*"      ERROR_OCCURED
*"----------------------------------------------------------------------
* 변수 선언

  TABLES : YTEST_ZLAB02.

  DATA : BEGIN OF i_zlab02 OCCURS 0.
          INCLUDE STRUCTURE YTEST_ZLAB02.
  DATA : END OF i_zlab02.

  DATA : tp_match LIKE sy-index,
         tp_txt   TYPE char200,
         tp_len   type sy-index.

  DATA: params LIKE pri_params,
        valid TYPE c.

  FIELD-SYMBOLS : <f1> LIKE i_zlab02-z_txt,
                  <f2> LIKE i_zlab02-z_txt,
                  <f3> LIKE i_zlab02-z_txt,
                  <f4> LIKE i_zlab02-z_txt,
                  <f5> LIKE i_zlab02-z_txt.

* 시작

* Data 집계 (해당 Application)
  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_zlab02
    FROM YTEST_ZLAB02
    WHERE zappl = p_zappl.

  IF sy-subrc <> 0.
    RAISE not_found.
    EXIT.
  ENDIF.

* 변수 Matching ZLAB02에 등록된 변수와 본 function의 변수를
* 연결 시킨다.
  CLEAR tp_match.
  LOOP AT i_zlab02 WHERE z_var = 'X'.
    tp_match = tp_match + 1.
    CASE tp_match.
      WHEN 1.
        ASSIGN (i_zlab02-z_txt) TO <f1>.
        ASSIGN p_var1 TO <f1>.
      WHEN 2.
        ASSIGN (i_zlab02-z_txt) TO <f2>.
        ASSIGN p_var2 TO <f2>.
      WHEN 3.
        ASSIGN (i_zlab02-z_txt) TO <f3>.
        ASSIGN p_var3 TO <f3>.
      WHEN 4.
        ASSIGN (i_zlab02-z_txt) TO <f4>.
        ASSIGN p_var4 TO <f4>.
      WHEN 5.
        ASSIGN (i_zlab02-z_txt) TO <f5>.
        ASSIGN p_var5 TO <f5>.
    ENDCASE.
  ENDLOOP.


* Printer Open
  NEW-PAGE PRINT ON
     NO-TITLE
     NO-HEADING
     LINE-SIZE 255
     DESTINATION 'ZRF_LABEL'
*     DESTINATION P_DEST
     IMMEDIATELY 'X'
     KEEP IN SPOOL 'X'
     NEW LIST IDENTIFICATION 'X'
     NO DIALOG.

* ZPL2 명령어를 만든다.
  CLEAR tp_match.

  LOOP AT i_zlab02.

*   WRITE : / i_zlab02-zcomm.

*   변수에다가 한글이 담겨서 들어오는 경우
    IF     i_zlab02-z_han = 'X' AND i_zlab02-z_var = 'X'.
      tp_match = tp_match + 1.
      CASE tp_match.
        WHEN 1.  tp_txt = <f1>.
        WHEN 2.  tp_txt = <f2>.
        WHEN 3.  tp_txt = <f3>.
        WHEN 4.  tp_txt = <f4>.
        WHEN 5.  tp_txt = <f5>.
      ENDCASE.
      CALL FUNCTION 'YTEST_Z_LABEL_001'
           EXPORTING
                p_hangul = tp_txt
                p_x      = i_zlab02-z_han_x
                p_y      = i_zlab02-z_han_y
                p_x_size = i_zlab02-z_han_x_size
                p_y_size = i_zlab02-z_han_y_size
                p_width  = i_zlab02-z_han_width.

*   그냥 한글을 적었을 경우
    ELSEIF i_zlab02-z_han = 'X' AND i_zlab02-z_var = ' '.
      tp_txt = i_zlab02-z_txt.
      CALL FUNCTION 'YTEST_Z_LABEL_001'
           EXPORTING
                p_hangul = tp_txt
                p_x      = i_zlab02-z_han_x
                p_y      = i_zlab02-z_han_y
                p_x_size = i_zlab02-z_han_x_size
                p_y_size = i_zlab02-z_han_y_size
                p_width  = i_zlab02-z_han_width.

*   변수로 들어올 경우
    ELSEIF i_zlab02-z_han = ' ' AND i_zlab02-z_var = 'X'.
      WRITE : / i_zlab02-zcomD no-gap.
      tp_match = tp_match + 1.
      CASE tp_match.
        WHEN 1.  WRITE : <f1> no-gap.
        WHEN 2.  WRITE : <f2> no-gap.
        WHEN 3.  WRITE : <f3> no-gap.
        WHEN 4.  WRITE : <f4> no-gap.
        WHEN 5.  WRITE : <f5> no-gap.
      ENDCASE.

*   그냥 명령문 후속 데이터일 경우
    ELSEIF i_zlab02-z_han = ' ' AND i_zlab02-z_var = ' '.
      WRITE : / i_zlab02-zcomD no-gap.
      tp_len = strlen( i_zlab02-z_txt ).
      check tp_len > 0.
      WRITE : i_zlab02-z_txt(tp_len) no-gap.
    ENDIF.

*   이미지 처리일 경우 (이미지 헥사 만드는법을 몰라용~~~)
*   나중에 다시 테스트 꼭 필요.
    IF i_zlab02-z_img = 'X'.

    ENDIF.

*   필드 종료 마크 삽입
    IF i_zlab02-z_fin = 'X'.
      WRITE : '^FS' no-gap.
    ENDIF.

  ENDLOOP.

  NEW-PAGE PRINT OFF.


ENDFUNCTION.
