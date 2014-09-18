FUNCTION Z_FCA_UPLOAD_FILE_FRNT_TO_APPL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_LOCAL_FILE_PATH) LIKE  RLGRAP-FILENAME
*"     VALUE(I_APPL_FILE_PATH) LIKE  RLGRAP-FILENAME
*"     VALUE(I_OVERWRITE) LIKE  BOOLE-BOOLE DEFAULT 'X'
*"  EXPORTING
*"     VALUE(RETURN) LIKE  BAPIRET2 STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
  DATA : LV_FRONT_FILE LIKE RCGFILETR-FTFRONT.
  DATA : LV_APPL_FILE  LIKE RCGFILETR-FTAPPL.
  DATA : LV_FILE_NAME  LIKE RLGRAP-FILENAME.  "//순수파일명(패쓰없음)
  DATA : LV_PATH_LEN   TYPE I.
  DATA : LV_LONG_FLAG VALUE ''.               "//FULLPATH 가 길경우 FLAG

  LV_APPL_FILE = I_APPL_FILE_PATH.
*-- LOCAL FILE명 만 별도로 분리 -> LV_FILE_NAME.
  PERFORM SPLIT_PATH_FILENAME USING I_LOCAL_FILE_PATH  "FULL PATH
                                    LV_FILE_NAME.      "파일명만

  CHECK RETURN IS INITIAL.

*-- 로컬파일 full path 길이.
  LV_PATH_LEN = STRLEN( I_LOCAL_FILE_PATH ).
*-- lv_path_len > 60 char. 의 경우 c:\ 로 일단 복사한다.
  IF LV_PATH_LEN > 60.
    PERFORM COPY_FILE_TO_LOCALL_ROOT USING I_LOCAL_FILE_PATH
                                           LV_FILE_NAME .

    CHECK RETURN IS INITIAL.

*-- 루트로 복사된 파일 패쓰(C:\파일명)을 업로드TYPE으로 변경
    LV_FRONT_FILE = I_LOCAL_FILE_PATH.  "// RCGFILETR-FTFRONT 형
*-- 파일LENGTH 가 길경우 FLAG.
    LV_LONG_FLAG = 'X'.
  ELSE.
    LV_FRONT_FILE = I_LOCAL_FILE_PATH.
  ENDIF.

**-- APP SRV PATH : /usr/sap/trans/파일 or others
**--- UPLOAD TO APP SERVER.


  CALL FUNCTION 'C13Z_FILE_UPLOAD_BINARY'
        EXPORTING
             I_FILE_FRONT_END   = LV_FRONT_FILE
             I_FILE_APPL        = LV_APPL_FILE
             I_FILE_OVERWRITE   = I_OVERWRITE
*       IMPORTING
*            E_FLG_OPEN_ERROR   =
*            E_OS_MESSAGE       =
        EXCEPTIONS
             FE_FILE_NOT_EXISTS = 1
             FE_FILE_READ_ERROR = 2
             AP_NO_AUTHORITY    = 3
             AP_FILE_OPEN_ERROR = 4
             AP_FILE_EXISTS     = 5
             OTHERS             = 6
                               .

  IF SY-SUBRC <> 0.
    PERFORM ERROR_MESSAGE USING sy-MSGID sy-MSGTY sy-MSGNO
                                SY-MSGV1 SY-MSGV2
                                SY-MSGV3 SY-MSGV4 RETURN.
  ENDIF.

  CHECK RETURN IS INITIAL.

*--- DELETE C:\ 의 파일-FULL PATH 가 길경우
*--- 임시로 복사했던 파일은 삭제한다.
  IF LV_LONG_FLAG = 'X'.

*-- remove file ----------------------
    CALL FUNCTION 'WS_FILE_DELETE'
         EXPORTING
              FILE    = I_LOCAL_FILE_PATH
*      IMPORTING
*           RETURN  =
         EXCEPTIONS
              OTHERS  = 1.
    IF SY-SUBRC <> 0.
      PERFORM ERROR_MESSAGE USING sy-MSGID sy-MSGTY sy-MSGNO
                                  SY-MSGV1 SY-MSGV2
                                  SY-MSGV3 SY-MSGV4 RETURN.
    ENDIF.

    CHECK RETURN IS INITIAL.

  ENDIF.
ENDFUNCTION.
