* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Conversion Payroll Results Load
* Version 1.0  - August 2000

* Qualification scales - table T77SK direct load
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZPT77SK46C MESSAGE-ID ZP.

TABLES :T77SK ,T77TS.    "create scales

PARAMETERS : FT77SK LIKE RLGRAP-FILENAME DEFAULT'C:\csi\t77sk.TXT'.

*data: begin of itabt77sk occurs 0.
*        include structure t77sk.
*data: text1(40).
*data :end of itabt77sk.
DATA: DELIMITER TYPE X VALUE'09', ERR(50).

PERFORM UPLOAD_T77SK USING FT77SK ERR.
*perform append.

*---------------------------------------------------------------------*
*       FORM UPLOAD_t77sk                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FT558B                                                        *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_T77SK USING FT77SK ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(65535),
   END OF ITAB.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME      = FT77SK
            FILETYPE      = 'ASC'
       TABLES
            DATA_TAB      = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
*  perform check_error using sy-subrc err.
  DATA : T.
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
       T77SK-SCALE_ID  "itabt77sk-scale_id
       T77TS-STEXT  "itabt77sk-text1
                 T.
  IF T77SK-SCALE_ID NE SPACE.
   INSERT T77SK.   "    append itabt77sk.
   IF SY-SUBRC NE 0.
    WRITE : / 'Tab T77SK insert error. Values ' , T77SK.
   ELSE.
    MOVE-CORRESPONDING T77SK TO T77TS.
    MOVE 'E' TO T77TS-LANGU.
    INSERT T77TS.
   ENDIF.
  ENDIF.
 CLEAR: T77SK , T77TS.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM APPEND                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*FORM APPEND.
*  LOOP AT ITABT77SK.
*    T77SK-SCALE_ID =  ITABT77SK-SCALE_ID.
*    T77SK-SCALE_ART =  ITABT77SK-SCALE_ART .
*    IF ITABT77SK-SCALE_ID NE SPACE.
*    INSERT  T77SK.
*    ENDIF.
*    IF SY-SUBRC NE 0.
*      WRITE:/ 'unable to insert into table t77sk'.
*      CONTINUE.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_ERROR                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ERR_CD                                                        *
*  -->  STAGE                                                         *
*---------------------------------------------------------------------*
FORM CHECK_ERROR USING ERR_CD STAGE.
  CASE ERR_CD.
    WHEN 0.
    WHEN OTHERS.
      WRITE:/ 'Error in the process ', STAGE, '. Error -', ERR_CD.
*      stop.
  ENDCASE.
ENDFORM.
