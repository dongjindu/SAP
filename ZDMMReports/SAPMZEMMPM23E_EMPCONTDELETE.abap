REPORT SAPMZEMMPM23E_EMPCONTDELETE MESSAGE-ID ZMMM.
************************************************************************
* Program Name      : SAPMZEMMPM23E_EMPCONTDELETE
* Author            : Haseeb Mohammad
* Creation Date     : 2006.12.01.
* Specifications By : Ben Worthy
* Pattern           : 1.0
* Development Request No :  UD1K922459
* Addl Documentation:
* Description       : Welcome Container Delete Edit.
*
************************************************************************
* Modification Logs
* Date        Developer    RequestNo    Description
*
************************************************************************

*&---------------------------------------------------------------------*
*&      DATA DECLERATION.
*&---------------------------------------------------------------------*

TABLES: ZTMM_CONTAINER, LAGP, T301.
DATA   : w_ok_code LIKE sy-ucomm,
         w_fcode   LIKE sy-ucomm,
         w_mode    type c,
         w_grey    type c,
         l_dyname like sy-repid,
         l_dynumb like sy-dynnr.


*DATA :  it_container like TABLE OF ztmm_container WITH HEADER LINE.
DATA :  it_container like ztmm_container .
DATA: contanr like IT_CONTAINER-CONT_REG_NUMB1.
DATA : BEGIN OF it_storage OCCURS 0,
         lgnum LIKE lagp-lgnum, " Warehouse Number
         lgtyp LIKE lagp-lgtyp, " Storage Type
         lgber LIKE lagp-lgber, " Storage section
         lgpla LIKE lagp-lgpla, " Storage bin
         kzler LIKE lagp-kzler, " Indicator empty
       END OF it_storage.

DATA: BEGIN OF it_scrfield OCCURS   0.
        INCLUDE STRUCTURE dynpread.
DATA: END OF it_scrfield.


*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1000 OUTPUT.
  SET TITLEBAR 'TITLE'.
  SET PF-STATUS 'MENU1000' EXCLUDING 'SAVE'.
  loop at screen.
    if screen-group4 eq 'GRP' OR screen-group4 eq 'BU1'.
      screen-input = 0.
      modify screen.
    endif.

  endloop.
  CASE w_mode.
    WHEN 'E' .
      SET PF-STATUS 'MENU1000'.
      SELECT SINGLE *  FROM ZTMM_CONTAINER WHERE
                   CONT_REG_NUMB1 = ZTMM_CONTAINER-CONT_REG_NUMB1.
       IF SY-SUBRC = 0.
         modify screen.
       ELSE.
        MESSAGE I009 WITH 'Container not found.  Check your entry. ' .
      ENDIF.
      loop at screen.
        if screen-group4 eq 'GRP'.
          screen-input = 1.
          modify screen.
        endif.
        if screen-group4 eq 'BU1' OR screen-group4 eq 'GP1'.
          screen-input = 0.
          modify screen.
        endif.

      endloop.

    WHEN 'D'.
      ZTMM_CONTAINER-CONT_REG_NUMB1 = ''.
      ZTMM_CONTAINER-LGPLA = ''.
      ZTMM_CONTAINER-LGBER = ''.
      ZTMM_CONTAINER-LGTYP = ''.
      ZTMM_CONTAINER-PASS_DATE = ''.
      ZTMM_CONTAINER-STATUS = ''.
      ZTMM_CONTAINER-FULLPER = ''.
      loop at screen.
        if screen-group4 eq 'GRP'.
          screen-input = 0.
          modify screen.
        endif.
      endloop.
      MODIFY SCREEN.

    WHEN 'A'.

      ZTMM_CONTAINER-CONT_REG_NUMB1 = IT_CONTAINER-CONT_REG_NUMB1.
      ZTMM_CONTAINER-LGPLA = IT_CONTAINER-LGPLA.
      ZTMM_CONTAINER-LGBER = IT_CONTAINER-LGBER.
      ZTMM_CONTAINER-LGTYP = IT_CONTAINER-LGTYP.
      ZTMM_CONTAINER-PASS_DATE = IT_CONTAINER-PASS_DATE.
      ZTMM_CONTAINER-STATUS = IT_CONTAINER-STATUS.
      ZTMM_CONTAINER-FULLPER = IT_CONTAINER-FULLPER.
      MODIFY SCREEN.
      LOOP AT SCREEN.
        if screen-group4 eq 'BU1'.
          screen-input = 1.
          modify screen.
        endif.
      ENDLOOP.
    WHEN 'N'.
      LOOP AT SCREEN.
        if screen-group4 eq 'BU1'.
          screen-input = 0.
          modify screen.
        endif.
      ENDLOOP.
    WHEN 'S'.
      LOOP AT SCREEN.
        if screen-group4 eq 'BU1'.
          screen-input = 1.
          modify screen.
        endif.
      ENDLOOP.
  ENDCASE.
  CLEAR IT_CONTAINER.
  clear w_mode.
ENDMODULE.                 " STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.
  DATA w_return type c.
  CASE SY-UCOMM.
    WHEN  'DEL'.
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
           EXPORTING
                DIAGNOSETEXT1  = 'Data will be deleted... Proceed?'
                DIAGNOSETEXT2  = ZTMM_CONTAINER-CONT_REG_NUMB1
                DIAGNOSETEXT3  = ''
                TEXTLINE1      = 'DELETE'
                TITEL          = 'Confirm Delete'
                START_COLUMN   = 25
                START_ROW      = 6
                CANCEL_DISPLAY = ''
           IMPORTING
                ANSWER         = w_return.
      IF W_RETURN = 'J'.
        DELETE FROM ZTMM_CONTAINER WHERE CONT_REG_NUMB1 =
                                    ZTMM_CONTAINER-CONT_REG_NUMB1.
        IF SY-SUBRC = 0.
          COMMIT WORK.
          w_mode = 'D'.
          MESSAGE I038 WITH ZTMM_CONTAINER-CONT_REG_NUMB1 .
        ELSE.
          MESSAGE I999 WITH 'Could not DELETE: '
                ZTMM_CONTAINER-CONT_REG_NUMB1 .
        ENDIF.
      ELSE.
        MESSAGE i999 WITH 'NO CHANGE MADE'.
      ENDIF.

    WHEN 'EDIT'.
      w_mode = 'E'.
      w_grey = 'E'.


*

*
*
*      CLEAR : it_scrfield, it_scrfield[].
*      it_scrfield-fieldname  = 'ZTMM_CONTAINER-CONT_REG_NUMB1'.
*      it_scrfield-fieldvalue = ZTMM_CONTAINER-CONT_REG_NUMB1.
*      APPEND it_scrfield.
*
*      l_dyname =  sy-repid.
*      l_dynumb = sy-dynnr.
*      CALL FUNCTION 'DYNP_VALUES_READ'
*           EXPORTING
*                DYNAME     = l_dyname
*                DYNUMB     = l_dynumb
*           TABLES
*                DYNPFIELDS = it_scrfield.
**
*      Loop at it_scrfield.
*        if it_scrfield-fieldname = 'ZTMM_CONTAINER-CONT_REG_NUMB1'.
*          ZTMM_CONTAINER-CONT_REG_NUMB1 = it_scrfield-fieldvalue.
*
*        endif.
*      endloop.
*      SELECT SINGLE * INTO IT_CONTAINER FROM ZTMM_CONTAINER WHERE
*                   CONT_REG_NUMB1 = ZTMM_CONTAINER-CONT_REG_NUMB1.
*      IF SY-SUBRC <> 0.
*        MESSAGE I009 WITH 'Container not found.  Check your entry. ' .
*      ENDIF.

    WHEN 'SAVE'.
      w_mode = 'S'.
      SELECT single * FROM T301 WHERE LGTYP = ZTMM_CONTAINER-LGTYP.
      IF SY-SUBRC <> 0.
        MESSAGE I009 WITH 'Container not found.  Check your entry. '.
        w_mode = 'N'.
      ELSE.
        IF ZTMM_CONTAINER-CONT_REG_NUMB1 = ''.
          MESSAGE I999 WITH 'PLEASE ENTER VALUES'.
          W_MODE = 'N'.
        ELSE.
          CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
               EXPORTING
                    DIAGNOSETEXT1  = 'Data for Container '
                    DIAGNOSETEXT2  = ZTMM_CONTAINER-CONT_REG_NUMB1
                    DIAGNOSETEXT3  = '... will be updated. '
                    TEXTLINE1      = 'Proceed?'
                    TITEL          = 'Confirm Change'
                    START_COLUMN   = 25
                    START_ROW      = 6
                    CANCEL_DISPLAY = ''
               IMPORTING
                    ANSWER         = w_return.
          IF W_RETURN = 'J'.

            if ZTMM_CONTAINER-FULLPER = 0.
              ZTMM_CONTAINER-FULLPER = 100.
            endif.

            update ZTMM_CONTAINER SET
             LGPLA = ZTMM_CONTAINER-LGPLA
             LGBER = ZTMM_CONTAINER-LGBER
             LGTYP = ZTMM_CONTAINER-LGTYP
             PASS_DATE = ZTMM_CONTAINER-PASS_DATE
             STATUS = ZTMM_CONTAINER-STATUS
             FULLPER = ZTMM_CONTAINER-FULLPER
             AEDAT = SY-DATUM
              AEZET = SY-UZEIT
              AENAM = SY-UNAME
             WHERE CONT_REG_NUMB1 = ZTMM_CONTAINER-CONT_REG_NUMB1.

*          MODIFY ZTMM_CONTAINER FROM ZTMM_CONTAINER.
            IF SY-SUBRC = 0.
**S> 08/04/11 Paul
       MESSAGE s037 WITH ZTMM_CONTAINER-CONT_REG_NUMB1 ' is saved'.
**E<
              W_MODE = 'N'.
            ELSE.
              MESSAGE I036 WITH ZTMM_CONTAINER-CONT_REG_NUMB1 .
            ENDIF.
          ELSE.
            SELECT SINGLE *  FROM ZTMM_CONTAINER WHERE
                   CONT_REG_NUMB1 = ZTMM_CONTAINER-CONT_REG_NUMB1.
            W_MODE = 'N'.

          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE PROGRAM.

    WHEN OTHERS.

      SELECT SINGLE * INTO IT_CONTAINER FROM ZTMM_CONTAINER WHERE
                   CONT_REG_NUMB1 = ZTMM_CONTAINER-CONT_REG_NUMB1.
      IF SY-SUBRC <> 0.
        MESSAGE I009 WITH 'Container not found.  Check your entry. ' .
        w_mode = 'N'.
      ELSE.
        w_mode = 'A'.
      ENDIF.
  ENDCASE.

  CLEAR SY-UCOMM.
  CLEAR w_return.
ENDMODULE.                 " USER_COMMAND_1000  INPUT

*&---------------------------------------------------------------------*
*& FORM  DISABLE_ELEMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISABLE_ELEMENTS USING GRP GP1 BU1.

  loop at screen.

    if screen-group4 eq 'GRP'.
      IF GRP EQ 1.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
*      modify screen.

    endif.

    if screen-group4 eq 'GP1'.
      IF GP1 EQ 1.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
*      modify screen.
    endif.


    if screen-group4 eq 'GP1'.
      IF BU1 EQ 1.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.

*      modify screen.
    endif.
    modify screen.

  endloop.

ENDFORM.                 " DISABLE_ELEMENTS
*&---------------------------------------------------------------------*
*&      Form  FILL_STORAGE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_storage_type.
  DATA : letyp LIKE lein-letyp.
  CLEAR : it_scrfield, it_scrfield[].
  it_scrfield-fieldname  = 'ZTMM_CONTAINER-LGTYP'.   "Storge type
  it_scrfield-fieldvalue = it_storage-lgtyp.
  APPEND it_scrfield.

  CLEAR it_scrfield.
  it_scrfield-fieldname  = 'ZTMM_CONTAINER-LGBER'.   "Storage section
  it_scrfield-fieldvalue = it_storage-lgber.
  APPEND it_scrfield.

  CLEAR it_scrfield.
  it_scrfield-fieldname  = 'ZTMM_CONTAINER-LGPLA'.   "Storage bin
  it_scrfield-fieldvalue = it_storage-lgpla.
  APPEND it_scrfield.


  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = it_scrfield
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.
  CHECK sy-subrc = 0.
  LOOP AT it_scrfield.
    CASE it_scrfield-fieldname.
      WHEN 'ZTMM_CONTAINER-LGTYP'.
        ZTMM_CONTAINER-LGTYP =
it_scrfield-fieldvalue.
      WHEN 'ZTMM_CONTAINER-LGBER'.
        ZTMM_CONTAINER-LGBER =
it_scrfield-fieldvalue.
      WHEN 'ZTMM_CONTAINER-LGPLA'.
        ZTMM_CONTAINER-LGPLA =
it_scrfield-fieldvalue.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " FILL_STORAGE_TYPE

*&---------------------------------------------------------------------*
*&      Module  NLTYP_POSSIBEL_ENTRY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE nltyp_possibel_entry INPUT.
  DATA : w_sfield   LIKE  help_info-fieldname,
         it_fields  LIKE  help_value OCCURS 1 WITH HEADER LINE,
         w_svalue   LIKE  help_info-fldvalue,
         w_idx    LIKE  sy-tabix          .
  data:     w_lgtyp      LIKE  lagp-lgtyp        .
  DATA : w_no TYPE i.

  IF W_GREY ='E'.
    w_lgtyp = '411'.

*  SELECT * FROM lagp
*           INTO CORRESPONDING FIELDS OF TABLE it_storage
*          WHERE lgnum = 'P01'
*            AND lgtyp IN (w_lgtyp , '411')
*            AND kzler = 'X'.

    SELECT * FROM lagp
               INTO CORRESPONDING FIELDS OF TABLE it_storage
              WHERE lgnum = 'P01'
                AND lgtyp IN ('411', '511', '421')
                AND kzler = 'X'.

    TABLES : ztmm_ct_errlog.

    DATA : it_ztmm_ct_errlog LIKE ztmm_ct_errlog OCCURS 0
                                                 WITH HEADER LINE.
    DATA : l_parking_txt LIKE ztmm_ct_errlog-parking_txt.

    CLEAR : it_storage, it_ztmm_ct_errlog, it_ztmm_ct_errlog[].

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_ct_errlog
             FROM ztmm_ct_errlog
             where flag ne 'X'.

    LOOP AT it_storage.
      CLEAR : l_parking_txt, it_ztmm_ct_errlog.
      CONCATENATE 'BB' '-' it_storage-lgtyp '-' it_storage-lgber
                       '-' it_storage-lgpla INTO l_parking_txt.
      READ TABLE it_ztmm_ct_errlog WITH KEY parking_txt = l_parking_txt.
      IF sy-subrc EQ 0.
        DELETE it_storage.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE it_storage LINES w_no.
    IF w_no = 0.
      MESSAGE e012.
    ENDIF.
* [ 2 ] Possible Entry
    CLEAR : it_fields, it_fields[].
    it_fields-tabname   = 'LAGP'.
    it_fields-fieldname = 'LGNUM'.
    it_fields-selectflag = ' '.
    APPEND it_fields.
    CLEAR : it_fields.
    it_fields-tabname   = 'LAGP'.
    it_fields-fieldname = 'LGTYP'.
    it_fields-selectflag = ' '.
    APPEND it_fields.
    CLEAR : it_fields.
    it_fields-tabname   = 'LAGP'.
    it_fields-fieldname = 'LGBER'.
    it_fields-selectflag = ' '.
    APPEND it_fields.
    CLEAR : it_fields.
    it_fields-tabname = 'LAGP'.
    it_fields-fieldname = 'LGPLA'.
    it_fields-selectflag = ' '.
    APPEND it_fields.
    CLEAR : it_fields.
    it_fields-tabname = 'LAGP'.
    it_fields-fieldname = 'KZLER'.
    it_fields-selectflag = ' '.
    APPEND it_fields.

    CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
         EXPORTING
              selectfield  = w_sfield
         IMPORTING
              ind          = w_idx
              select_value = w_svalue
         TABLES
              fields       = it_fields
              full_table   = it_storage.
    IF sy-subrc <> 0.
    ENDIF.
    CHECK w_idx > 0 . "AND w_mode = 'C'.
    CLEAR it_storage.
    READ TABLE it_storage INDEX w_idx.
    PERFORM fill_storage_type.
  ENDIF.
ENDMODULE.                 " NLTYP_POSSIBEL  INPUT
