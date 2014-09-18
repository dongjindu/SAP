************************************************************************
* Program Name      : ZAPP_VIN_VERSION_UPDATE_TEMP
* Author            :
* Creation Date     : Chris Li
* Specifications By : Chris Li
* Development Request No : UD1K916226
* Addl Documentation:
* Description       : check the vehicle versin and corresponding word
*                     order version
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************


REPORT zapp_vin_version_update_temp  MESSAGE-ID zmpp LINE-SIZE 110
     NO STANDARD PAGE HEADING .
TABLES: ausp.
TYPES: BEGIN OF t_vin,
        objek  LIKE ausp-objek,
        atinn  LIKE ausp-atinn,
        atwrt  LIKE ausp-atwrt,
       END OF t_vin.

DATA: it_vin TYPE t_vin OCCURS 0 WITH HEADER LINE.

DATA: it_wo  TYPE t_vin OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_result OCCURS 0,
        vin    LIKE ausp-objek,
        vvers  LIKE ausp-atwrt,
        worder LIKE ausp-objek,
        wvers  LIKE ausp-atwrt,
        messg(50) TYPE c,
      END OF it_result.
data: l_answer.
DATA: W_UPDATE .

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: p_objek FOR ausp-objek .
SELECTION-SCREEN END OF BLOCK B1.


START-OF-SELECTION.
  PERFORM read_vin_version.
  PERFORM check_vin_version.
*  PERFORM update_vin_version.

END-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  PERFORM display_result.


AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'UPDATE'.
     perform pop_confirm using l_answer.
     if l_answer = '1'.
       PERFORM update_vin_version.
       LEAVE TO LIST-PROCESSING.
       PERFORM DISPLAY_RESULT.
     endif.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  read_vin_version
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vin_version.
  DATA: l_atinn LIKE ausp-atinn,
        l_version LIKE ausp-atwrt,
        l_atnam LIKE cabn-atnam.
  DATA: BEGIN OF lt_wo OCCURS 0,
         objek  LIKE ausp-objek,
         atwrt  LIKE ausp-objek,
        END OF lt_wo.

* read characteric number
  l_atnam = 'P_WORK_ORDER'.
  PERFORM read_characteristic USING l_atnam l_atinn.

* reading work order
  SELECT objek atwrt
    INTO CORRESPONDING FIELDS OF TABLE lt_wo
    FROM ausp
    WHERE klart = '002'
     AND  objek IN p_objek
     AND atinn = l_atinn.
* read characteric number
  l_atnam = 'P_VERSION'.
  PERFORM read_characteristic USING l_atnam l_atinn.

  LOOP AT lt_wo.

    it_result-vin     = lt_wo-objek.
    it_result-worder    = lt_wo-atwrt.
*   get the vin version.

    SELECT SINGLE atwrt INTO l_version
      FROM ausp
      WHERE objek = lt_wo-objek
       AND  klart = '002'
       AND  atinn = l_atinn.
    IF sy-subrc = 0.
      it_result-vvers = l_version.
    ENDIF.
*   get the work order version.
    SELECT SINGLE atwrt INTO l_version
      FROM ausp
      WHERE klart = '001'
        AND objek = lt_wo-atwrt
        AND atinn = l_atinn.
    IF sy-subrc = 0.
      it_result-wvers = l_version.
    ENDIF.

    APPEND it_result.
    CLEAR: it_result.

  ENDLOOP.


ENDFORM.                    " read_vin_version
*&---------------------------------------------------------------------*
*&      Form  check_vin_version
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vin_version.
  LOOP AT it_result.
    IF it_result-vvers EQ it_result-wvers.
      DELETE it_result.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_vin_version
*&---------------------------------------------------------------------*
*&      Form  update_vin_version
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_vin_version.
  DATA: l_atinn LIKE cabn-atinn.

  PERFORM read_characteristic USING 'P_VERSION' l_atinn.

  LOOP AT it_result.
    UPDATE ausp SET atwrt = it_result-wvers
        WHERE objek = it_result-vin
         AND  klart = '002'
         AND  atinn = l_atinn.
    IF sy-subrc NE 0.
      it_result-messg = 'Failed update'.
    ELSE.
      it_result-messg = 'Updated'.
    ENDIF.
    MODIFY it_result.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.                    " update_vin_version



*&---------------------------------------------------------------------*
*&      Form  display_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_result.
  DATA: i_line_group TYPE i.
  DATA: i_skip TYPE i.
  DATA: i_lines(4) TYPE n.
  DATA: l_text(100).

  SORT it_result BY vin.

  DESCRIBE TABLE it_result LINES i_lines.

  l_text = 'The Following vehivles have wrong version ('.
  CONCATENATE l_text i_lines ')' INTO l_text.
  WRITE:/(110) sy-uline.
  WRITE: / l_text.
  WRITE:/(110) sy-uline.




  IF i_lines > 0.
    WRITE: /0(15) text-011, 16(10) text-012, 27(18) text-013,
           46(10) text-014, 60(50) text-015.
    WRITE: / sy-uline.

*     INITIALIZE THE COUNTER
    i_line_group = 1.
    i_lines = 0.

    LOOP AT it_result.
      i_lines = i_lines + 1.
      WRITE: /0(15) it_result-vin,16(10) it_result-vvers ,
             27(18) it_result-worder,46(10) it_result-wvers,
             60(50) it_result-messg.

      i_skip = 5 * i_line_group.
      IF i_lines EQ i_skip.
        SKIP 1.
        i_line_group = i_line_group + 1.
      ENDIF.

    ENDLOOP.
  ELSE.
    WRITE: / text-020.
  ENDIF.
  WRITE:/(110) sy-uline.
ENDFORM.                    " display_result
*&---------------------------------------------------------------------*
*&      Form  read_characteristic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ATNAM  text
*      -->P_L_ATINN  text
*----------------------------------------------------------------------*
FORM read_characteristic USING    p_atnam
                                  p_atinn.

  SELECT SINGLE atinn INTO p_atinn
    FROM cabn
    WHERE atnam = p_atnam.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH 'Wrong characteric name, please check'.
  ENDIF.

ENDFORM.                    " read_characteristic
*&---------------------------------------------------------------------*
*&      Form  pop_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ANSWER  text
*----------------------------------------------------------------------*
form pop_confirm using    p_answer.
  data: l_text(30).

  l_text = 'Do you want change VIN version?'.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR                    = 'Please confirm'
*     DIAGNOSE_OBJECT             = ' '
      text_question               = l_text
*     TEXT_BUTTON_1               = 'Ja'(001)
*     ICON_BUTTON_1               = ' '
*     TEXT_BUTTON_2               = 'Nein'(002)
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
   IMPORTING
      ANSWER                      = p_answer
*   TABLES
*     PARAMETER                   =
   EXCEPTIONS
     TEXT_NOT_FOUND              = 1
     OTHERS                      = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

endform.                    " pop_confirm
