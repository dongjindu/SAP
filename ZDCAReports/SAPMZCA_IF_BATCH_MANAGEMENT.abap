************************************************************************
* Program Name      : SAPMZCA_IF_BATCH_MANAGEMENT
* Author            : ByungSung, Bae
* Creation Date     : 2003.09.22.
* Specifications By : ByungSung, Bae
* Development Request No :
* Addl Documentation:
* Description       : Interface & Bauchjob
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZCA_IF_BATCH_MANAGEMENT   .
TABLES: USR01,
        TSTC,
        TBTCO,
        ZSCA_IF_SECTION_1000,         "Screen 1000 Structure
        ZSCA_IF_SECTION_2000,         "Screen 2000 Structure
        ZTCA_IF_SECTION.              "Interface Section

*----- Internal Tables
DATA: IT_2000 LIKE ZSCA_IF_SECTION_2000 OCCURS 0 WITH HEADER LINE.

*----- Ranges
RANGES: R_UNAME   FOR   ZSCA_IF_SECTION_1000-UNAME,
        R_IFTYP   FOR   ZSCA_IF_SECTION_1000-IFTYP,
        R_TCODE   FOR   ZSCA_IF_SECTION_1000-TCODE_FR,
        R_JOBNAME FOR   ZSCA_IF_SECTION_1000-JOBNAME_FR.

*----- Table controls
CONTROLS : TC_2000 TYPE TABLEVIEW USING SCREEN 2000.

*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  CASE SY-DYNNR.
    WHEN 1000.
      SET PF-STATUS '1000'.
      SET TITLEBAR  '1000'.
    WHEN 2000.
      SET PF-STATUS '2000'.
    WHEN 3000.
      SET PF-STATUS '3000'.
    WHEN 4000.
      SET PF-STATUS '4000'.
    WHEN 5000.
      SET PF-STATUS '5000'.
  ENDCASE.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_FIELD_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_FIELD_1000 INPUT.
*----- Check screen input value & fill RANGES variables
  PERFORM CHECK_UNAME.
  PERFORM CHECK_IFTYP.
  PERFORM CHECK_TCODE.
  PERFORM CHECK_JOBNAME.
ENDMODULE.                 " CHECK_FIELD_1000  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_UNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_UNAME.
  CLEAR: R_UNAME, R_UNAME[].

  IF ZSCA_IF_SECTION_1000-UNAME NE ''.
    IF ZSCA_IF_SECTION_1000-UNAME CS '*'.
      MOVE: 'CP'                       TO R_UNAME-OPTION.
    ELSE.
      MOVE: 'EQ'                       TO R_UNAME-OPTION.
    ENDIF.

    MOVE: 'I'                        TO R_UNAME-SIGN,
          ZSCA_IF_SECTION_1000-UNAME TO R_UNAME-LOW.

    APPEND R_UNAME.
  ENDIF.

  SELECT SINGLE * FROM USR01 WHERE BNAME IN R_UNAME.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M01.
  ENDIF.
ENDFORM.                    " check_UNAME
*&---------------------------------------------------------------------*
*&      Form  check_iftyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_IFTYP.
  CLEAR: R_IFTYP, R_IFTYP[].

  IF ZSCA_IF_SECTION_1000-IFTYP NE ''.
    MOVE: 'I'                        TO R_IFTYP-SIGN,
          'EQ'                       TO R_IFTYP-OPTION,
          ZSCA_IF_SECTION_1000-IFTYP TO R_UNAME-LOW.

    APPEND R_IFTYP.
  ENDIF.
ENDFORM.                    " check_iftyp
*&---------------------------------------------------------------------*
*&      Form  check_tcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_TCODE.
  CLEAR: R_TCODE, R_TCODE[].

  IF    ZSCA_IF_SECTION_1000-TCODE_FR EQ '' AND
        ZSCA_IF_SECTION_1000-TCODE_TO EQ ''.

  ELSEIF ZSCA_IF_SECTION_1000-TCODE_FR NE '' AND
         ZSCA_IF_SECTION_1000-TCODE_TO EQ ''.
    IF ZSCA_IF_SECTION_1000-TCODE_FR CS '*'.
      MOVE: 'CP'                       TO R_TCODE-OPTION.
    ELSE.
      MOVE: 'EQ'                       TO R_TCODE-OPTION.
    ENDIF.

    MOVE: 'I'                           TO R_TCODE-SIGN,
          ZSCA_IF_SECTION_1000-TCODE_FR TO R_TCODE-LOW.

    APPEND R_TCODE.
  ELSEIF ZSCA_IF_SECTION_1000-TCODE_FR EQ '' AND
         ZSCA_IF_SECTION_1000-TCODE_TO NE ''.
    MOVE: 'I'                           TO R_TCODE-SIGN,
          'BT'                          TO R_TCODE-OPTION,
          ZSCA_IF_SECTION_1000-TCODE_TO TO R_TCODE-HIGH.

    APPEND R_TCODE.
  ELSEIF ZSCA_IF_SECTION_1000-TCODE_FR NE '' AND
         ZSCA_IF_SECTION_1000-TCODE_TO NE ''.
    MOVE: 'I'                           TO R_TCODE-SIGN,
          'BT'                          TO R_TCODE-OPTION,
          ZSCA_IF_SECTION_1000-TCODE_FR TO R_TCODE-LOW,
          ZSCA_IF_SECTION_1000-TCODE_TO TO R_TCODE-HIGH.

    APPEND R_TCODE.
  ENDIF.


  SELECT SINGLE * FROM TSTC WHERE TCODE IN R_TCODE.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M02.
  ENDIF.
ENDFORM.                    " check_tcode
*&---------------------------------------------------------------------*
*&      Form  check_jobname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_JOBNAME.
  CLEAR: R_JOBNAME,R_JOBNAME[].

  IF    ZSCA_IF_SECTION_1000-JOBNAME_FR EQ '' AND
        ZSCA_IF_SECTION_1000-JOBNAME_TO EQ ''.

  ELSEIF ZSCA_IF_SECTION_1000-JOBNAME_FR NE '' AND
         ZSCA_IF_SECTION_1000-JOBNAME_TO EQ ''.
    IF ZSCA_IF_SECTION_1000-JOBNAME_FR CS '*'.
      MOVE: 'CP'                       TO R_JOBNAME-OPTION.
    ELSE.
      MOVE: 'EQ'                       TO R_JOBNAME-OPTION.
    ENDIF.

    MOVE: 'I'                             TO R_JOBNAME-SIGN,
          ZSCA_IF_SECTION_1000-JOBNAME_FR TO R_JOBNAME-LOW.

    APPEND R_JOBNAME.
  ELSEIF ZSCA_IF_SECTION_1000-JOBNAME_FR EQ '' AND
         ZSCA_IF_SECTION_1000-JOBNAME_TO NE ''.
    MOVE: 'I'                             TO R_JOBNAME-SIGN,
          'BT'                            TO R_JOBNAME-OPTION,
          ZSCA_IF_SECTION_1000-JOBNAME_TO TO R_JOBNAME-HIGH.

    APPEND R_JOBNAME.
  ELSEIF ZSCA_IF_SECTION_1000-JOBNAME_FR NE '' AND
         ZSCA_IF_SECTION_1000-JOBNAME_TO NE ''.
    MOVE: 'I'                             TO R_JOBNAME-SIGN,
          'BT'                            TO R_JOBNAME-OPTION,
          ZSCA_IF_SECTION_1000-JOBNAME_FR TO R_JOBNAME-LOW,
          ZSCA_IF_SECTION_1000-JOBNAME_TO TO R_JOBNAME-HIGH.

    APPEND R_JOBNAME.
  ENDIF.


  SELECT SINGLE * FROM TBTCO WHERE JOBNAME IN R_JOBNAME.
  IF SY-SUBRC NE 0.
    MESSAGE i000(ZZ) WITH TEXT-M03.
  ENDIF.
ENDFORM.                    " check_jobname
*&---------------------------------------------------------------------*
*&      Module  user_command_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR: SY-UCOMM.
      LEAVE TO SCREEN 0.
    WHEN 'EXEC'.
      CLEAR: SY-UCOMM.
      PERFORM EXECUTE_1000_RTN.
  ENDCASE.
ENDMODULE.                 " user_command_1000  INPUT
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_1000_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXECUTE_1000_RTN.
  CLEAR: IT_2000, IT_2000[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_2000
    FROM ZTCA_IF_SECTION
   WHERE UNAME   IN R_UNAME
     AND IFTYP   IN R_IFTYP
     AND TCODE   IN R_TCODE
     AND JOBNAME IN R_JOBNAME.
  IF SY-SUBRC NE 0.
    MESSAGE S000(ZZ) WITH TEXT-M04.
  ENDIF.

  CALL SCREEN 2000.
ENDFORM.                    " EXECUTE_1000_RTN
*&---------------------------------------------------------------------*
*&      Module  move_itab_to_screen_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOVE_ITAB_TO_SCREEN_2000 OUTPUT.
  READ TABLE IT_2000 INDEX TC_2000-CURRENT_LINE.
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING : IT_2000 TO ZSCA_IF_SECTION_2000.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.
ENDMODULE.                 " move_itab_to_screen_2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  get_itab_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ITAB_LINES OUTPUT.
  DESCRIBE TABLE IT_2000 LINES TC_2000-LINES.
ENDMODULE.                 " get_itab_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE SY-UCOMM.
    WHEN : 'EXIT' OR 'CANC'.
      CLEAR  SY-UCOMM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  move_screen_to_itab_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOVE_SCREEN_TO_ITAB_2000 INPUT.
  READ TABLE IT_2000 INDEX TC_2000-CURRENT_LINE.

  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M05.
  ENDIF.

  MOVE : ZSCA_IF_SECTION_2000-CHECK TO IT_2000-CHECK.

  MODIFY IT_2000 INDEX TC_2000-CURRENT_LINE.
ENDMODULE.                 " move_screen_to_itab_2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR: SY-UCOMM.
      LEAVE TO SCREEN 0.
    WHEN 'CREATE'.
      CLEAR: SY-UCOMM.
      PERFORM CREATE_RTN.
    WHEN 'CHANGE'.
      CLEAR: SY-UCOMM.
      PERFORM CHANGE_RTN.
    WHEN 'DELETE'.
      CLEAR: SY-UCOMM.
      PERFORM DELETE_RTN.
  ENDCASE.
ENDMODULE.                 " user_command_2000  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_RTN.
  CLEAR: ZSCA_IF_SECTION_2000.

  CALL SCREEN 3000 STARTING AT  20 5
                   ENDING   AT 100 15.
ENDFORM.                    " CREATE_RTN
*&---------------------------------------------------------------------*
*&      Module  check_field_3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_FIELD_3000 INPUT.
*----- Check screen input value & fill RANGES variables
  PERFORM CHECK_TCODE_3000.
  PERFORM CHECK_JOBNAME_3000.
  PERFORM CHECK_UNAME_3000.
  PERFORM CHECK_RETRY_N_DISTC.
ENDMODULE.                 " check_field_3000  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_TCODE_3000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_TCODE_3000.
  SELECT SINGLE * FROM ZTCA_IF_SECTION
                 WHERE TCODE = ZSCA_IF_SECTION_2000-TCODE.
  IF SY-SUBRC EQ 0.
    MESSAGE E000(ZZ) WITH TEXT-M06.
  ENDIF.
ENDFORM.                    " CHECK_TCODE_3000
*&---------------------------------------------------------------------*
*&      Form  check_jobname_3000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_JOBNAME_3000.
  SELECT SINGLE * FROM TBTCO
                 WHERE JOBNAME = ZSCA_IF_SECTION_2000-JOBNAME.
  IF SY-SUBRC NE 0.
    MESSAGE i000(ZZ) WITH TEXT-M03.
  ENDIF.
ENDFORM.                    " check_jobname_3000
*&---------------------------------------------------------------------*
*&      Form  check_uname_3000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_UNAME_3000.
  SELECT SINGLE * FROM USR01 WHERE BNAME = ZSCA_IF_SECTION_2000-UNAME.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M01.
  ENDIF.
ENDFORM.                    " check_uname_3000
*&---------------------------------------------------------------------*
*&      Form  check_RETRY_n_DISTC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RETRY_N_DISTC.
  IF ZSCA_IF_SECTION_2000-RETRY IS INITIAL AND
     ZSCA_IF_SECTION_2000-DISTC IS INITIAL.
    MESSAGE E000(ZZ) WITH TEXT-M07.
  ENDIF.
ENDFORM.                    " check_RETRY_n_DISTC
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_3000 INPUT.
  CASE SY-UCOMM.
    WHEN 'SAVE'.
      PERFORM SAVE_3000_RTN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_3000_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_3000_RTN.
  MOVE: SY-DATUM TO ZSCA_IF_SECTION_2000-ERDAT,
        SY-UZEIT TO ZSCA_IF_SECTION_2000-ERZET,
        SY-UNAME TO ZSCA_IF_SECTION_2000-ERNAM,
        SY-DATUM TO ZSCA_IF_SECTION_2000-AEDAT,
        SY-UZEIT TO ZSCA_IF_SECTION_2000-AEZET,
        SY-UNAME TO ZSCA_IF_SECTION_2000-AENAM.

  MOVE-CORRESPONDING ZSCA_IF_SECTION_2000 TO ZTCA_IF_SECTION.

  INSERT ZTCA_IF_SECTION.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M08.
  ENDIF.

  COMMIT WORK AND WAIT.

  CLEAR: IT_2000.
  MOVE-CORRESPONDING ZTCA_IF_SECTION TO IT_2000.
  APPEND IT_2000.

  MESSAGE S000(ZZ) WITH TEXT-M09.
  LEAVE TO SCREEN 0.
ENDFORM.                    " SAVE_3000_RTN
*&---------------------------------------------------------------------*
*&      Module  check_field_4000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_FIELD_4000 INPUT.
*----- Check screen input value & fill RANGES variables
  PERFORM CHECK_JOBNAME_3000.
  PERFORM CHECK_UNAME_3000.
  PERFORM CHECK_RETRY_N_DISTC.
ENDMODULE.                 " check_field_4000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_4000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_4000 INPUT.
  CASE SY-UCOMM.
    WHEN 'SAVE'.
      PERFORM SAVE_4000_RTN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_4000  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_4000_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_4000_RTN.
  MOVE: SY-DATUM TO ZSCA_IF_SECTION_2000-AEDAT,
        SY-UZEIT TO ZSCA_IF_SECTION_2000-AEZET,
        SY-UNAME TO ZSCA_IF_SECTION_2000-AENAM.

  MOVE-CORRESPONDING ZSCA_IF_SECTION_2000 TO ZTCA_IF_SECTION.

  UPDATE ZTCA_IF_SECTION.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M10.
  ENDIF.

  COMMIT WORK AND WAIT.

  READ TABLE IT_2000 WITH KEY TCODE = ZSCA_IF_SECTION_2000-TCODE.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M05.
  ENDIF.

  MOVE-CORRESPONDING ZTCA_IF_SECTION TO IT_2000.
  MODIFY IT_2000 INDEX SY-TABIX.

  MESSAGE S000(ZZ) WITH TEXT-M09.
  LEAVE TO SCREEN 0.
ENDFORM.                    " save_4000_rtn
*&---------------------------------------------------------------------*
*&      Form  change_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_RTN.
  CLEAR: ZSCA_IF_SECTION_2000.

  READ TABLE IT_2000 WITH KEY CHECK = 'X'.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M11.
  ENDIF.

  MOVE IT_2000 TO ZSCA_IF_SECTION_2000.

  CALL SCREEN 4000 STARTING AT  20 5
                   ENDING   AT 100 15.
ENDFORM.                    " change_rtn
*&---------------------------------------------------------------------*
*&      Form  delete_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_RTN.
  READ TABLE IT_2000 WITH KEY CHECK = 'X'.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M11.
  ENDIF.

  MOVE IT_2000 TO ZSCA_IF_SECTION_2000.

  CALL SCREEN 5000 STARTING AT  20 5
                   ENDING   AT 100 15.

ENDFORM.                    " delete_rtn
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_5000 INPUT.
  CASE SY-UCOMM.
    WHEN 'DELETE'.
      PERFORM DELETE_4000_RTN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_5000  INPUT
*&---------------------------------------------------------------------*
*&      Form  DELETE_4000_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_4000_RTN.
  DATA: LW_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            DEFAULTOPTION = 'A'
            TEXTLINE1     = TEXT-B02
            TITEL         = TEXT-B01
       IMPORTING
            ANSWER        = LW_ANSWER.

  CASE LW_ANSWER.
    WHEN 'A'.
      LEAVE TO SCREEN 0.
    WHEN 'J'.
      DELETE FROM ZTCA_IF_SECTION
       WHERE TCODE = ZSCA_IF_SECTION_2000-TCODE.
      IF SY-SUBRC NE 0.
        MESSAGE E000(ZZ) WITH TEXT-B03.
      ENDIF.

      DELETE IT_2000 WHERE TCODE = ZSCA_IF_SECTION_2000-TCODE.
      IF SY-SUBRC NE 0.
        MESSAGE E000(ZZ) WITH TEXT-M11.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'N'.
  ENDCASE.
ENDFORM.                    " DELETE_4000_rtn
