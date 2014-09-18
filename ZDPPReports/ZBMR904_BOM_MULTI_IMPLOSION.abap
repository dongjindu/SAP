REPORT ZBMR904_BOM_MULTI_IMPLOSION MESSAGE-ID ZMPP
                   NO STANDARD PAGE HEADING.

************************************************************************
* Program Name      : ZBMR904_BOM_MULTI_IMPLOSION
* Author            : Yongping
* Creation Date     : 2004.09.13.
* Specifications By : Yongping
* Pattern           :
* Development Request No : UD1K912192
* Addl Documentation:
* Description       : Purchase order part indented BOM check list
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
*****************************************************************
*GLOBAL DATA
*****************************************************************
TABLES: MARA.

DATA: OK_CODE LIKE SY-UCOMM.
DATA: OK_SAVE LIKE SY-UCOMM.
DATA: S_ERROR .  " 'X': ERROR OCCURED
DATA: S_MESSAGE(50) TYPE C.

DATA: C_PLANT_E001(4) TYPE C VALUE 'E001'.
DATA: C_PLANT_E002(4) TYPE C VALUE 'E002'.

DATA: I_LEVEL TYPE I.
DATA: I_PRE_LEVEL TYPE I.
DATA: I_LINES TYPE I.
DATA: L_VALID_FROM LIKE SY-DATUM.
DATA: L_VALID_TO LIKE SY-DATUM.
DATA: L_MAT_0 TYPE MARA-MATNR.
DATA: L_MAT_1 TYPE MARA-MATNR.
DATA: L_MAT_2 TYPE MARA-MATNR.
DATA: L_MAT_3 TYPE MARA-MATNR.
DATA: L_MAT_4 TYPE MARA-MATNR.
DATA: L_MAT_5 TYPE MARA-MATNR.

CONTROLS: TC_RESULT TYPE TABLEVIEW USING SCREEN 100.
DATA: IO_TOTAL(10) TYPE C.
****************************************************************
*INTERNAL TABLES
****************************************************************

* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        MATNR TYPE MARA-MATNR,  "MATERIAL
      END   OF IT_EXCL.

*OUTPUT FORMAT
DATA: BEGIN OF IT_RESULT OCCURS 0,
        MATNR LIKE MARA-MATNR,
        FLAG  TYPE C,
        NEXT1 LIKE MARA-MATNR,
        NEXT2 LIKE MARA-MATNR,
        NEXT3 LIKE MARA-MATNR,
        NEXT4 LIKE MARA-MATNR,
        NEXT5 LIKE MARA-MATNR,
      END OF IT_RESULT.
DATA: BEGIN OF IT_MATERIAL OCCURS 0,
        MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
        WERKS TYPE MARC-WERKS,
        DATUV TYPE STPO-DATUV,
      END OF IT_MATERIAL.
DATA: IT_IMPLOSION LIKE IT_MATERIAL OCCURS 1 WITH HEADER LINE.
DATA: IT_NEXT_LEVEL_MAT LIKE IT_MATERIAL OCCURS 1 WITH HEADER LINE.
DATA: IT_RESULT_TEMP LIKE IT_RESULT OCCURS 0 WITH HEADER LINE.

*****************************************************************
*END OF  DATA DECLARATION
*****************************************************************


*****************************************************************
*SELECTION-SCREEN
*****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_CASE1 RADIOBUTTON  GROUP RG USER-COMMAND UCOM    .
SELECTION-SCREEN COMMENT  (40) TEXT-002 FOR FIELD P_CASE1  .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  10(10) TEXT-003 FOR FIELD P_MATNR .
SELECT-OPTIONS: P_MATNR  FOR  MARA-MATNR.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.


SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_CASE2    RADIOBUTTON  GROUP RG DEFAULT 'X'   .
SELECTION-SCREEN COMMENT  (40) TEXT-004 FOR FIELD P_CASE2  .
SELECTION-SCREEN END OF LINE.

PARAMETERS:
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT'.
SELECTION-SCREEN END OF BLOCK B1.
**********************************************************************
*END OF SELECTION SCREEN
**********************************************************************

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.



START-OF-SELECTION.
  CASE 'X'.
    WHEN P_CASE1.  "MATERIAL NUMBER
      PERFORM PART_GET.
    WHEN P_CASE2.  "EXCEL FILE
      REFRESH IT_EXCL. CLEAR IT_EXCL.
      PERFORM UPLOAD_PROCESS.
  ENDCASE.


* CHECK IF PARTS HAVE FSC
   IF S_ERROR <> 'X'.
     PERFORM FIND_FSC.
   ENDIF.
END-OF-SELECTION.
 IF S_ERROR = 'X'.
   WRITE: / S_MESSAGE.
 ELSE.
   PERFORM WRITE_PROCESS.
 ENDIF.




*********************************************************************
**** FORMS
*********************************************************************
FORM screen_modify.
  LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'P_FILETY'.
          screen-input = 0.
      ENDCASE.
    MODIFY SCREEN.
    CLEAR screen.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  tmp_mask = ',*.*,*.*.'.

  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = p_file
            def_path         = def_path
*           MASK             = ',*.*,*.*.'
            mask             = tmp_mask
            mode             = mode
*           TITLE            = ' '
       IMPORTING
            filename         = tmp_filename
*         RC               =
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
* IF SY-SUBRC = 1.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  PART_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PART_GET.
   SELECT MARA~MATNR MARA~MTART MARC~WERKS
    INTO CORRESPONDING FIELDS OF TABLE IT_MATERIAL
    FROM MARA INNER JOIN MARC
    ON MARA~MATNR EQ MARC~MATNR
    WHERE MARA~MATNR IN P_MATNR.
   IF SY-SUBRC <> 0.
     S_ERROR = 'X'.
     S_MESSAGE = 'NO DATA FOUND !'.
     EXIT.
   ENDIF.
*  DELETE FSC MATERIAL
   DELETE IT_MATERIAL WHERE MTART EQ 'FERT'.
** for E002
*   DELETE IT_MATERIAL WHERE WERKS EQ C_PLANT_E001.
   DELETE IT_MATERIAL WHERE WERKS EQ C_PLANT_E001
                         OR WERKS EQ C_PLANT_E002.
ENDFORM.                    " PART_GET



*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM upload_process.

  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            codepage                = ' '
            filename                = p_file
            filetype                = p_filety
*           HEADLEN                 = ' '
*           LINE_EXIT               = ' '
*           TRUNCLEN                = ' '
*           USER_FORM               = ' '
*           USER_PROG               = ' '
*      IMPORTING
*           FILELENGTH              =
       TABLES
            data_tab                = it_excl
      EXCEPTIONS
           conversion_error        = 1
           file_open_error         = 2
           file_read_error         = 3
           invalid_table_width     = 4
           invalid_type            = 5
           no_batch                = 6
           unknown_error           = 7
           gui_refuse_filetransfer = 8
           customer_error          = 9
           OTHERS                  = 10
            .
  CASE sy-subrc.
    WHEN 0.
*     READ THE PLANT AND TYPE INFORMATION FROM MATERIAL MASTER
      SELECT MARA~MATNR MARA~MTART MARC~WERKS
        INTO CORRESPONDING FIELDS OF TABLE IT_MATERIAL
        FROM MARA INNER JOIN MARC
        ON MARA~MATNR EQ MARC~MATNR
        FOR ALL ENTRIES IN IT_EXCL
        WHERE MARA~MATNR = IT_EXCL-MATNR.
      IF SY-SUBRC <> 0.
        S_ERROR = 'X'.
        S_MESSAGE = 'ERROR OCCURED WHEN READ PLANT AND TYPE'.
        EXIT.
      ENDIF.
** FOR E002
*      DELETE IT_MATERIAL WHERE MATNR = 'FERT' OR
*                               WERKS = C_PLANT_E001.
      DELETE IT_MATERIAL WHERE MATNR = 'FERT' OR
                               WERKS = C_PLANT_E001 OR
                               WERKS = C_PLANT_E002.
** END
      EXIT.
    WHEN 1.
        S_ERROR = 'X'.
        S_MESSAGE = 'FILE CONVERSION ERROR !'.
        EXIT.
    WHEN 2.
        S_ERROR = 'X'.
        S_MESSAGE = 'FILE OPEN ERROR, FILE NO FOUND!'.
        EXIT.
    WHEN 3.
        S_ERROR = 'X'.
        S_MESSAGE = 'FILE READ ERROR'.
        EXIT.
    WHEN OTHERS.
        S_ERROR = 'X'.
        S_MESSAGE = 'FILE UPLOAD ERROR, CHECK YOUR FILE!.'.
        EXIT.
    ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
*       OUTPUT THE CHECK RESULT
*----------------------------------------------------------------------*
*  -->   INPUT INTERNAL TABLE IT_RESULT
*  <--   OUTPUT TO TABLE CONTROL
*----------------------------------------------------------------------*
FORM WRITE_PROCESS.
  SORT IT_RESULT BY FLAG MATNR .
  DESCRIBE TABLE IT_RESULT LINES I_LINES.
  IO_TOTAL = I_LINES.
  CALL SCREEN 100.

ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  FIND_FSC
*&---------------------------------------------------------------------*
*       IMPLODE THE MATERIAL BOM AND CHECK IF FSC EXISTS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_FSC.
  DATA: P_DATE LIKE SY-DATUM.
  DATA: P_STATUS . " 'X':FOUND FSC, INITIAL: NO FSC
*  MAKE UP THE VALID-FROM DATE FOR BOM IMPLOSION
*  NOW WE USE THE CURRENT DATE AS VALID DATE
*   CLEAR IT_MATERIAL.
*   IT_MATERIAL-DATUV = SY-DATUM.
*   MODIFY IT_MATERIAL FROM IT_MATERIAL
*     TRANSPORTING DATUV
*     WHERE DATUV EQ SPACE.
   P_DATE = SY-DATUM.
   L_VALID_FROM = SY-DATUM.
   L_VALID_TO = SY-DATUM.
*  LOOP IT_MATERIAL TO CHECK EACH MATERIAL
   LOOP AT IT_MATERIAL.
*    MAKE THE PARAMETERS FOR FORM LOOK_FOR_FSC.
     CLEAR IT_IMPLOSION.
     REFRESH IT_IMPLOSION.
     MOVE-CORRESPONDING IT_MATERIAL TO IT_IMPLOSION.
     MOVE P_DATE TO IT_IMPLOSION-DATUV.
     APPEND IT_IMPLOSION.
     CLEAR IT_IMPLOSION.
     CLEAR I_LEVEL.
     CLEAR I_PRE_LEVEL.
     CLEAR P_STATUS.
     CLEAR L_MAT_0.
     CLEAR L_MAT_1.
     CLEAR L_MAT_2.
     CLEAR L_MAT_3.
     CLEAR L_MAT_4.
     CLEAR L_MAT_5.

*    CHECK IF FSC EXIST FOR THIS MATERIAL
     PERFORM LOOK_FOR_FSC TABLES IT_IMPLOSION
                          CHANGING P_STATUS.
*    CHECK THE STATUS AND STORE THE RESULT
     IT_RESULT-MATNR = IT_MATERIAL-MATNR.
     IF P_STATUS = 'X'.
       IT_RESULT-FLAG = '1'.
     ELSE.
       IT_RESULT-FLAG = '2'.
     ENDIF.
     IT_RESULT-NEXT1 = L_MAT_1.
     IT_RESULT-NEXT2 = L_MAT_2.
     IT_RESULT-NEXT3 = L_MAT_3.
     IT_RESULT-NEXT4 = L_MAT_4.
     IT_RESULT-NEXT5 = L_MAT_5.
     APPEND IT_RESULT.

   ENDLOOP.
ENDFORM.                    " FIND_FSC
*&---------------------------------------------------------------------*
*&      Form  LOOK_FOR_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IMPLOSION - MATERIAL LIST TO BE CHECKED FOR FSC
*      <--P_STATUS --'X': FSC EXIST, INITIAL: NO FSC
*----------------------------------------------------------------------*
FORM LOOK_FOR_FSC TABLES   P_IMPLOSION STRUCTURE IT_IMPLOSION
                  CHANGING P_STATUS.


      I_LEVEL = I_LEVEL + 1.  " IMPLOSION LEVEL
      I_PRE_LEVEL = I_LEVEL - 1.

     LOOP AT P_IMPLOSION.
*      WHENEVER THE P_STATUS IS 'X', STOP SEARCHING
       IF P_STATUS EQ 'X'.
          EXIT.
       ENDIF.
*      STORE THE PREVIOUS LEVEL MATERIAL
       CASE I_PRE_LEVEL.
         WHEN '0'.
          L_MAT_0 = P_IMPLOSION-MATNR.
         WHEN '1'.
          L_MAT_1 = P_IMPLOSION-MATNR.
         WHEN '2'.
          L_MAT_2 = P_IMPLOSION-MATNR.
         WHEN '3'.
          L_MAT_3 = P_IMPLOSION-MATNR.
         WHEN '4'.
          L_MAT_4 = P_IMPLOSION-MATNR.
         WHEN '5'.
          L_MAT_5 = P_IMPLOSION-MATNR.
         WHEN OTHERS.
          MESSAGE I000 WITH 'MORE THAN 5 LEVEL BOM STRUCTURE!'.
       ENDCASE.
       PERFORM BOM_IMPLODE_CHECK USING P_IMPLOSION-MATNR
                                       P_IMPLOSION-WERKS
                                       P_IMPLOSION-DATUV
                                 CHANGING P_STATUS.
       IF P_STATUS = 'X'.
*       STORE THE RESULT AND EXIT THIS LOOP??????????
        CASE I_LEVEL.
           WHEN '0'.
            L_MAT_0 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '1'.
            L_MAT_1 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '2'.
            L_MAT_2 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '3'.
            L_MAT_3 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '4'.
            L_MAT_4 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '5'.
            L_MAT_5 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN OTHERS.
            MESSAGE I000 WITH 'MORE THAN 5 LEVEL BOM STRUCTURE!'.
         ENDCASE.
         EXIT.
       ELSEIF P_STATUS = 'N'.  "NO PARENT FOUND
         CONTINUE.
       ELSE.
         CASE I_LEVEL."STORE THE LAST PARENT
           WHEN '0'.
            L_MAT_0 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '1'.
            L_MAT_1 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '2'.
            L_MAT_2 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '3'.
            L_MAT_3 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '4'.
            L_MAT_4 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN '5'.
            L_MAT_5 = IT_NEXT_LEVEL_MAT-MATNR.
           WHEN OTHERS.
            MESSAGE I000 WITH 'WE COULD NOT HAVE MORE THAN 5 LEVEL!'.
         ENDCASE.


       ENDIF.


*    CHECK IF PARENT EXIST
       DESCRIBE TABLE IT_NEXT_LEVEL_MAT LINES I_LINES.
       IF I_LINES = 0. "NO MORE PARENT
         EXIT.
       ELSE.
*       CALL THIS PERFORM TO CHECK THE PARENT LEVEL RECURSIVELY
        CLEAR P_STATUS.
        PERFORM LOOK_FOR_FSC TABLES IT_NEXT_LEVEL_MAT
                           CHANGING P_STATUS.
      ENDIF.

     ENDLOOP.


ENDFORM.                    " LOOK_FOR_FSC
*&---------------------------------------------------------------------*
*&      Form  BOM_IMPLODE_CHECK
*&---------------------------------------------------------------------*
*       IMPLODE THE MATEIAL AND CEHCK IF FSC PARENT EXISTS.
*----------------------------------------------------------------------*
*      -->P_MATNR  MATERIAL NUMBER
*      -->P_WERKS  PLANT
*      -->P_DATUV  VALID-FROM DATE AND VALID-TO DATE
*      <--P_STATUS : 'X'-FOUND FSC ; INITIAL: NO FSC
*----------------------------------------------------------------------*
FORM BOM_IMPLODE_CHECK USING    P_MATNR
                                P_WERKS
                                P_DATUV
                       CHANGING P_STATUS.

  DATA : IT_MC29S   LIKE MC29S   OCCURS 0 WITH HEADER LINE,
       IT_STPOV   LIKE STPOV   OCCURS 0 WITH HEADER LINE,
       IT_CSCEQUI LIKE CSCEQUI OCCURS 0 WITH HEADER LINE,
       IT_CSCKND  LIKE CSCKND  OCCURS 0 WITH HEADER LINE,
       IT_CSCMAT  LIKE CSCMAT  OCCURS 0 WITH HEADER LINE,
       IT_CSCSTD  LIKE CSCSTD  OCCURS 0 WITH HEADER LINE,
       IT_CSCTPL  LIKE CSCTPL  OCCURS 0 WITH HEADER LINE.


  call function 'CS_WHERE_USED_MAT'
       exporting
            datub                      = L_VALID_FROM
            datuv                      = L_VALID_TO
            matnr                      = p_MATNR
            werks                      = p_werks
            STLAN                      = '1'
       importing
            topmat                     = it_mc29s
       tables
            wultb                      = it_stpov
            equicat                    = it_cscequi
            kndcat                     = it_cscknd
            matcat                     = it_cscmat
            stdcat                     = it_cscstd
            tplcat                     = it_csctpl
       exceptions
            call_invalid               = 1
            material_not_found         = 2
            no_where_used_rec_found    = 3
            no_where_used_rec_selected = 4
            no_where_used_rec_valid    = 5
            others                     = 6.
      IF SY-SUBRC <> 0.
        CASE SY-SUBRC.
          WHEN 1. " CALL ERROR
            MESSAGE E000 WITH 'BOM IMPLOSION CALL ERROR!'.
          WHEN OTHERS.  " NO PARENTS
            P_STATUS = 'N'.  " NO PARENTS
            EXIT.
        ENDCASE.
      ENDIF.
*     STORE THE IMPLOSION RESULT

*     READ THE MATERIAL TYPE FROM MARA

      CLEAR IT_NEXT_LEVEL_MAT.
      REFRESH IT_NEXT_LEVEL_MAT.
      SELECT MATNR MTART
        INTO CORRESPONDING FIELDS OF TABLE IT_NEXT_LEVEL_MAT
        FROM MARA
        FOR ALL ENTRIES IN IT_STPOV
        WHERE MATNR = IT_STPOV-MATNR.

*     LOOP THE IMPLOSION RESULT TO CHECK FSC MATERIAL
      LOOP AT IT_NEXT_LEVEL_MAT.
         READ TABLE IT_STPOV WITH KEY MATNR = IT_NEXT_LEVEL_MAT-MATNR.
         IT_NEXT_LEVEL_MAT-WERKS = IT_STPOV-WERKS.
         IT_NEXT_LEVEL_MAT-DATUV = IT_STPOV-DATUV.
         MODIFY IT_NEXT_LEVEL_MAT.
         IF IT_NEXT_LEVEL_MAT-MTART EQ 'FERT'.
            P_STATUS = 'X'.
            EXIT.
         ENDIF.
      ENDLOOP.

ENDFORM.                    " BOM_IMPLODE_CHECK
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TITLE_100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  OK_SAVE = OK_CODE.
  CLEAR OK_CODE.
  CASE OK_SAVE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
