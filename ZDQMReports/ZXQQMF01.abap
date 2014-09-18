*----------------------------------------------------------------------*
***INCLUDE ZXQQMF01 .
*----------------------------------------------------------------------*
*<<<<<<<< Start of EQM01 - ZQMEX_02 QM-Notification Enhancement >>>>>>
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT_FOR_TEXT_EDITOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TE_0101_A  text
*      -->P_TEC_0101_A  text
*      -->P_0019   text
*----------------------------------------------------------------------*
FORM CREATE_OBJECT_FOR_TEXT_EDITOR
       TABLES PT_EDITOR  STRUCTURE IT_EDITOR
              PT_TLINE   STRUCTURE TLINE
       USING  P_TE   TYPE REF TO CL_GUI_TEXTEDIT
              P_TEC  TYPE REF TO CL_GUI_CUSTOM_CONTAINER
              VALUE(P_CC) .                     "// Custom Control Name


  CHECK P_TEC IS INITIAL.

*-- Create Container for Editor Using Custom Control
  CREATE OBJECT P_TEC
    EXPORTING
      CONTAINER_NAME              = P_CC
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6
      .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*-- Create Editor in Editor Container. P_TEC

  CREATE OBJECT P_TE
     EXPORTING
       PARENT          = P_TEC
       WORDWRAP_MODE   = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
*       WORDWRAP_MODE   = CL_GUI_TEXTEDIT=>wordwrap_off
       WORDWRAP_POSITION = 45 "80
       WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE
     EXCEPTIONS
       ERROR_CNTL_CREATE = 1
       ERROR_CNTL_INIT   = 2
       ERROR_CNTL_LINK   = 3
       ERROR_DP_CREATE   = 4
       OTHERS            = 5
       .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*//--- Set Long Text to Editor


***

  CHECK SY-TCODE NE 'QM01'.

*-- Get long text for Notification change/display Transaction.
  CASE P_CC.
    WHEN 'CC_0101_A'.
* Begin of changes - UD1K940788
      PERFORM RETRIEVE_TEXT_TO_ITAB TABLES PT_EDITOR
                                           PT_TLINE
                                    USING  C_TDID_DS
                                           VIQMEL-QMNUM.
* END of changes - UD1K940788

*
*    WHEN 'CC_0101_B'.
*      PERFORM RETRIEVE_TEXT_TO_ITAB TABLES PT_EDITOR
*                                           PT_TLINE
*                                    USING  C_TDID_CT
*                                           VIQMEL-QMNUM.
  ENDCASE.

  CHECK NOT PT_EDITOR[] IS INITIAL.

  PERFORM SET_TEXT_TO_EDITOR  TABLES  PT_EDITOR
                              USING   P_TE        .


ENDFORM.                    " CREATE_OBJECT_FOR_TEXT_EDITOR
*&---------------------------------------------------------------------*
*&      Form  SET_EDITOR_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TE_0101_A  text
*      -->P_C_READ_ONLY  text
*----------------------------------------------------------------------*
FORM SET_EDITOR_MODE USING    P_TE TYPE REF TO CL_GUI_TEXTEDIT
                              P_MODE   .

  CHECK NOT P_TE IS INITIAL.

*----- Editor의 모드를 제어한다.
  CALL METHOD P_TE->SET_READONLY_MODE
    EXPORTING
      READONLY_MODE = P_MODE
    EXCEPTIONS
      ERROR_CNTL_CALL_METHOD = 1
      INVALID_PARAMETER      = 2.

ENDFORM.                    " SET_EDITOR_MODE
*&---------------------------------------------------------------------*
*&      Form  SET_TEXT_TO_EDITOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_EDITOR  text
*      -->P_P_TE  text
*----------------------------------------------------------------------*
FORM SET_TEXT_TO_EDITOR
                    TABLES P_IT   STRUCTURE IT_EDITOR
                    USING  P_TE   TYPE REF TO CL_GUI_TEXTEDIT .

  CALL METHOD P_TE->SET_TEXT_AS_R3TABLE
      EXPORTING
          TABLE = P_IT[]
      EXCEPTIONS
          OTHERS = 1.



ENDFORM.                    " SET_TEXT_TO_EDITOR
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTE_TEXTEDITOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TE  text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTE_TEXTEDITOR
                        USING  P_TE   TYPE REF TO CL_GUI_TEXTEDIT.

  CALL METHOD P_TE->SET_TOOLBAR_MODE
    EXPORTING
      TOOLBAR_MODE           = CL_GUI_TEXTEDIT=>FALSE
    EXCEPTIONS
      ERROR_CNTL_CALL_METHOD = 1
      INVALID_PARAMETER      = 2
      OTHERS                 = 3
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD P_TE->SET_STATUSBAR_MODE
    EXPORTING
      STATUSBAR_MODE         = CL_GUI_TEXTEDIT=>FALSE
    EXCEPTIONS
      ERROR_CNTL_CALL_METHOD = 1
      INVALID_PARAMETER      = 2
      OTHERS                 = 3
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " SET_ATTRIBUTE_TEXTEDITOR
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT_EDITOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EDITOR_A  text
*      -->P_IT_TLINE_A  text
*      -->P_TE_0101_A  text
*----------------------------------------------------------------------*
FORM GET_TEXT_EDITOR TABLES   PT_EDITOR STRUCTURE IT_EDITOR
                              PT_TLINE  STRUCTURE TLINE
                     USING    P_TE   TYPE REF TO CL_GUI_TEXTEDIT.

  REFRESH: PT_EDITOR, PT_TLINE.
  CLEAR : PT_EDITOR, PT_TLINE.

  CALL METHOD P_TE->GET_TEXT_AS_R3TABLE
    IMPORTING
      TABLE                  = PT_EDITOR[]
    EXCEPTIONS
      ERROR_DP               = 1
      ERROR_CNTL_CALL_METHOD = 2
      ERROR_DP_CREATE        = 3
      POTENTIAL_DATA_LOSS    = 4
      OTHERS                 = 5
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK NOT PT_EDITOR[] IS INITIAL.

  LOOP AT PT_EDITOR.
    CLEAR PT_TLINE.
    AT FIRST.
      MOVE : '*' TO PT_TLINE-TDFORMAT.
    ENDAT.

    MOVE : PT_EDITOR-LINE TO PT_TLINE-TDLINE.
    APPEND PT_TLINE.

  ENDLOOP.


ENDFORM.                    " GET_TEXT_EDITOR
*&---------------------------------------------------------------------*
*&      Form  RETRIEVE_TEXT_TO_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_EDITOR  text
*      -->P_PT_TLINE  text
*      -->P_0133   text
*      -->P_VIQMEL_QMNUM  text
*----------------------------------------------------------------------*
FORM RETRIEVE_TEXT_TO_ITAB TABLES   PT_EDITOR STRUCTURE IT_EDITOR
                                    PT_TLINE  STRUCTURE TLINE
                           USING    VALUE(P_ID)
                                    P_QMNUM.
  DATA : LW_STXH LIKE STXH.

  CLEAR PT_EDITOR. REFRESH PT_EDITOR.

  CLEAR THEAD.
  THEAD-TDOBJECT = C_TDOBJECT.   " Long Text Object
  THEAD-TDNAME   = P_QMNUM.  " Name
  THEAD-TDSPRAS  = SY-LANGU.
  THEAD-TDID  = P_ID.        "ID

*-- Check existence of long text.
  SELECT SINGLE * INTO LW_STXH
      FROM STXH
        WHERE TDOBJECT = THEAD-TDOBJECT
          AND TDNAME   = THEAD-TDNAME
          AND TDID     = THEAD-TDID
          AND TDSPRAS  = THEAD-TDSPRAS.

  CHECK SY-SUBRC = 0.

*----- Read Longtext object into internal table
  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            CLIENT                  = SY-MANDT
            ID                      = THEAD-TDID
            LANGUAGE                = THEAD-TDSPRAS
            NAME                    = THEAD-TDNAME
            OBJECT                  = THEAD-TDOBJECT
       TABLES
            LINES                   = PT_TLINE
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK     SY-SUBRC = 0 AND
        NOT PT_TLINE[] IS INITIAL.

  LOOP AT PT_TLINE.
    CLEAR PT_EDITOR.
    MOVE PT_TLINE-TDLINE TO PT_EDITOR-LINE.
    APPEND PT_EDITOR.
  ENDLOOP.


ENDFORM.                    " RETRIEVE_TEXT_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  SAVE_TEXT_FOR_CORRECTIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE_A  text
*      -->P_0026   text
*      -->P_I_VIQMEL_QMNUM  text
*----------------------------------------------------------------------*
FORM SAVE_TEXT_FOR_CORRECTIVE     TABLES   PT_TLINE STRUCTURE TLINE
                                  USING    P_TDID
                                           VALUE(P_QMNUM).



  CLEAR THEAD.
  THEAD-TDOBJECT = C_TDOBJECT.   " Long Text Object
  THEAD-TDNAME   = P_QMNUM.  " Name
  THEAD-TDSPRAS  = SY-LANGU.
  THEAD-TDID  = P_TDID.        "ID

  CALL FUNCTION 'SAVE_TEXT'
       EXPORTING
            CLIENT   = SY-MANDT
            HEADER   = THEAD
       TABLES
            LINES    = PT_TLINE
       EXCEPTIONS
            ID       = 1
            LANGUAGE = 2
            NAME     = 3
            OBJECT   = 4
            OTHERS   = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  CHECK SY-SUBRC = 0.
*  COMMIT WORK.

ENDFORM.                    " SAVE_TEXT_FOR_CORRECTIVE
*&---------------------------------------------------------------------*
*&      Form  GET_MAKTX_OF_MATNR_FERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSQM_CI_QMEL_VEHICLE  text
*      -->P_ZSQM_CI_QMEL_MAKTX  text
*----------------------------------------------------------------------*
FORM GET_MAKTX_OF_MATNR_FERT USING    P_MATNR TYPE MATNR
                                      P_MAKTX TYPE MAKTX.

  SELECT SINGLE MAKTX INTO P_MAKTX
    FROM ZVQM_MATNR_VH
       WHERE VEHICLE = P_MATNR
         AND MTART   = C_MTART_FERT.

ENDFORM.                    " GET_MAKTX_OF_MATNR_FERT
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT_OF_CODE_QPCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KATART_OC  text
*      -->P_CODEGRP_OC  text
*      -->P_CODE_OC  text
*      <--P_KURZTEXT_OC  text
*----------------------------------------------------------------------*
FORM GET_TEXT_OF_CODE_QPCT USING    P_KATART
                                    P_CODEGRP
                                    P_CODE
                           CHANGING P_KURZTEXT.

  SELECT SINGLE KURZTEXT_C INTO P_KURZTEXT
     FROM ZVQM_OCCR_LOC
       WHERE KATALOGART  = P_KATART
         AND CODEGRUPPE  = P_CODEGRP
         AND CODE        = P_CODE.

ENDFORM.                    " GET_TEXT_OF_CODE_QPCT
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT_OF_TWEWT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSQM_CI_QMEL_EXTWG  text
*      <--P_ZSQM_CI_QMEL_EWBEZ  text
*----------------------------------------------------------------------*
FORM GET_TEXT_OF_TWEWT USING    P_EXTWG
                       CHANGING P_EWBEZ.

  SELECT SINGLE EWBEZ INTO P_EWBEZ
      FROM ZVQM_TWEWT
        WHERE EXTWG = P_EXTWG.

ENDFORM.                    " GET_TEXT_OF_TWEWT
*&---------------------------------------------------------------------*
*&      Form  GET_EXTERNAL_MAT_GRP_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VIQMEL_MATNR  text
*      <--P_ZSQM_CI_QMEL_EXTWG  text
*----------------------------------------------------------------------*
FORM GET_EXTERNAL_MAT_GRP_CODE USING    P_MATNR  TYPE MATNR
                               CHANGING P_EXTWG.

  CHECK NOT P_MATNR IS INITIAL.

  SELECT SINGLE EXTWG   INTO P_EXTWG
     FROM MARA
       WHERE MATNR = P_MATNR.

  CHECK SY-SUBRC NE 0.
  CLEAR P_EXTWG.

ENDFORM.                    " GET_EXTERNAL_MAT_GRP_CODE
*<<<<<<<< End of EQM01 - ZQMEX_02 >>>>>>
*&---------------------------------------------------------------------*
*&      Form  save_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_TEXT.

  DATA: W_QMNUM LIKE VIQMEL-QMNUM.

**-- Import Long Text from ABAP memory
*---  Description of improvement  data.
  IMPORT IT_TLINE_A FROM MEMORY ID 'ZQMELDI'.
*---  Content of Confirmation data.
*  IMPORT IT_TLINE_B FROM MEMORY ID 'ZQMELCC'.

*-----                                        HASEEB Modified
  FREE MEMORY ID 'ZQMELDI'.
  FREE MEMORY ID 'ZQMELCC'.
*-----                                            End Haseeb.
*&------                                                Shiva
  GET PARAMETER ID 'IQM' FIELD W_QMNUM.
*&-----                                              end shiva
  IF  W_QMNUM IS INITIAL.
    W_QMNUM = WI_QMNUM.
  ENDIF.
** Changed by Furong on 02/24/09
  IF SY-TCODE <> 'QM01'.
    CLEAR THEAD.
    THEAD-TDOBJECT = 'QMEL'.   " Long Text Object
    THEAD-TDNAME   = W_QMNUM.  " Name
    THEAD-TDSPRAS  = SY-LANGU.
    THEAD-TDID  = 'LTQM'.

    CALL FUNCTION 'READ_TEXT'
         EXPORTING
              CLIENT                  = SY-MANDT
              ID                      = THEAD-TDID
              LANGUAGE                = THEAD-TDSPRAS
              NAME                    = THEAD-TDNAME
              OBJECT                  = THEAD-TDOBJECT
         TABLES
              LINES                   = IT_TLINE_B
         EXCEPTIONS
              ID                      = 1
              LANGUAGE                = 2
              NAME                    = 3
              NOT_FOUND               = 4
              OBJECT                  = 5
              REFERENCE_CHECK         = 6
              WRONG_ACCESS_TO_ARCHIVE = 7
              OTHERS                  = 8.
    IF SY-SUBRC <> 0 and not IT_TLINE_B[] is initial.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
** End of change


**--- Description of improvement  dat
  IF NOT IT_TLINE_A[] IS INITIAL.
* Begin of changes - UD1K940659
*    PERFORM SAVE_TEXT_FOR_CORRECTIVE
*                        TABLES  IT_TLINE_A      "/Text Table
*                        USING   C_TDID_DS       "/Text ID
*                                w_qmnum.        "/Notification No.
*    E_VIQMEL-LTEXT_IMPR = 'X'. "/Long text exist
********modif by 100565
*  ELSE.
*    PERFORM SAVE_TEXT_FOR_CORRECTIVE
*                        TABLES  IT_TLINE_A      "/Text Table
*                        USING   C_TDID_DS       "/Text ID
*                                w_qmnum.        "/Notification No.
**    E_VIQMEL-LTEXT_IMPR = ' '. "/Long text exist
********end modif 100565
  ENDIF.
**---  Content of Confirmation data.
  IF NOT IT_TLINE_B[] IS INITIAL.
* Do not save TEXT when called from ZQMP12 transaction once again
    CHECK  SY-BINPT  EQ ''.                                 "UD1K940788
    PERFORM SAVE_TEXT_FOR_CORRECTIVE
                        TABLES  IT_TLINE_B      "/Text Table
                        USING   C_TDID_CT       "/Text ID
                                W_QMNUM.        "/Notification No.
*    E_VIQMEL-LTEXT_CONF = 'X'.
********modif by 100565              "/Long text exist
*  ELSE.
*    PERFORM SAVE_TEXT_FOR_CORRECTIVE
*                        TABLES  IT_TLINE_B      "/Text Table
*                        USING   C_TDID_CT       "/Text ID
*                                w_qmnum.        "/Notification No.
**    E_VIQMEL-LTEXT_CONF = ' '. "/Long text exist
********end modif 100565
  ENDIF.

ENDFORM.                    " save_text

*&---------------------------------------------------------------------*
*&      FORM  CHECK_NOTI_PORTAL     , Haseeb created.
*&---------------------------------------------------------------------*
*       Check all fiels of ztpm_noti_portal table files for notification
*and display appropriate message if the fields are empty while saving,
*and donot save until fields are filled.
*----------------------------------------------------------------------*
FORM CHECK_NOTI_PORTAL.
  DATA: WA_TLINE LIKE TLINE.
  DATA: IT_TLINES LIKE TABLE OF WA_TLINE,
        W_TDNAME LIKE THEAD-TDNAME.
  W_TDNAME = VIQMEL-QMNUM.
  DATA : ZC TYPE C.
*  TABLES: ZTQM_NOTI_PORTAL.
**VIQMEL-QMNUM,VIQMEL-QMART
*  select single * from ZTQM_NOTI_PORTAL where
*  qmart = viqmel-qmart and Qmnum = Viqmel-qmnum.
  IF ZSQM_CI_QMEL-PLANDAT IS INITIAL OR
     ZSQM_CI_QMEL-COMPLETED IS INITIAL.
  ELSE.
    IF       ZTQM_NOTI_PORTAL-ZAT_HMMA IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZAT_TIER1 IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZIN_TRANSIT IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZOTHER IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZGOOD_PART_IDEN IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZINSP_AGREEMENT IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZWORK_INST IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZPFMEA IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZDFMEA IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZCONTROL_PLAN IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZPROCESS_FLOW IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZOTHERS IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZCLEAN_DATE IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZPER_INSPECTED IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZPER_INSPECTED IS INITIAL OR
             ZTQM_NOTI_PORTAL-ZPER_DEFECTED  IS INITIAL.

      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.

    ENDIF.

    IF ZSQM_CI_QMEL-RESPONSIVE IS INITIAL.
      MESSAGE W000(ZMQM) WITH 'Supplier Responsiveness not Completed'.
    ENDIF.

    IF
*       ZSQM_CI_QMEL-LNDWNTIME IS INITIAL OR
*       ZSQM_CI_QMEL-CODEGRP_VH IS INITIAL OR
*       ZSQM_CI_QMEL-CODE_VH IS INITIAL OR
*       ZSQM_CI_QMEL-KURZTEXT_VH IS INITIAL OR
*       ZSQM_CI_QMEL-EXTWG IS INITIAL OR
*       ZSQM_CI_QMEL-EWBEZ IS INITIAL OR
       ZSQM_CI_QMEL-SENDDAT IS INITIAL .
*       OR        ZSQM_CI_QMEL-SUCCESS IS INITIAL.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_DATA_LOSS'
*           EXPORTING
*                DEFAULTOPTION = 'J'
*                TITEL         = 'Containment at HMMA is not filled'
*                START_COLUMN  = 25
*                START_ROW     = 6
*           importing
*                ANSWER = zc.

      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.
*      exit.
    ENDIF.
    PERFORM READ_TEXT TABLES IT_TLINES USING 'ZQM1' W_TDNAME 'ZQMEL'.
    IF IT_TLINES[] IS INITIAL.
      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.
    ENDIF.
    PERFORM READ_TEXT TABLES IT_TLINES USING 'ZQM2' W_TDNAME 'ZQMEL'.
    IF IT_TLINES[] IS INITIAL.
      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.
    ENDIF.
    PERFORM READ_TEXT TABLES IT_TLINES USING 'ZQM3' W_TDNAME 'ZQMEL'.
    IF IT_TLINES[] IS INITIAL.
      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.
    ENDIF.
    PERFORM READ_TEXT TABLES IT_TLINES USING 'ZQM4' W_TDNAME 'ZQMEL'.
    IF IT_TLINES[] IS INITIAL.
      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.
    ENDIF.
    PERFORM READ_TEXT TABLES IT_TLINES USING 'ZQM5' W_TDNAME 'ZQMEL'.
    IF IT_TLINES[] IS INITIAL.
      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.
    ENDIF.
    PERFORM READ_TEXT TABLES IT_TLINES USING 'ZQM6' W_TDNAME 'ZQMEL'.
    IF IT_TLINES[] IS INITIAL.
      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.
    ENDIF.
    PERFORM READ_TEXT TABLES IT_TLINES USING 'ZQM7' W_TDNAME 'ZQMEL'.
    IF IT_TLINES[] IS INITIAL.
      MESSAGE E004(ZMQM) WITH 'Containment at HMMA is not filled'.
    ENDIF.
  ENDIF.

ENDFORM.   "END OF CHECK_NOTI_PORTAL
*&---------------------------------------------------------------------*
*&      Form  CALL_ZQMP12
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ZQMP12.
  SET PARAMETER ID 'LIF' FIELD VIQMEL-LIFNUM.
  SET PARAMETER ID 'QMR' FIELD 'Q2'.
  SET PARAMETER ID 'IQM' FIELD VIQMEL-QMNUM.
*  LEAVE TO LIST-PROCESSING.
  CALL TRANSACTION 'ZQMP12' AND SKIP FIRST SCREEN .

*      SUBMIT "SAPMZRQM25R_NOTI_SUMMARY_P" VIA SELECTION-SCREEN
*                AND RETURN.


*           USING VIQMEL  .
*
*      CALL TRANSACTION 'SE38'  USING BDCDATA  MODE 'N'
*                         MESSAGES INTO ITAB.
*
ENDFORM.                    " CALL_ZQMP12
*&---------------------------------------------------------------------*
*&      Form  read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINES  text
*      -->P_0049   text
*      -->P_W_TDNAME  text
*      -->P_0051   text
*----------------------------------------------------------------------*
FORM READ_TEXT TABLES P_IT_TLINES STRUCTURE TLINE
               USING  P_TDID
                      P_TDNAME
                      P_TDOBJECT.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*   CLIENT                        = SY-MANDT
      ID                            = P_TDID
      LANGUAGE                      = SY-LANGU
      NAME                          = P_TDNAME
      OBJECT                        = P_TDOBJECT
* IMPORTING
*   HEADER                        =
    TABLES
      LINES                         = P_IT_TLINES
 EXCEPTIONS
   ID                            = 1
   LANGUAGE                      = 2
   NAME                          = 3
   NOT_FOUND                     = 4
   OBJECT                        = 5
   REFERENCE_CHECK               = 6
   WRONG_ACCESS_TO_ARCHIVE       = 7
   OTHERS                        = 8 .

ENDFORM.                    " read_text
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_EMAIL TABLES PT_MESS
                USING: P_EMAIL P_EMAIL2 P_QMNUM
                       P_MESS1 P_MESS2.

  DATA:   IT_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
          IT_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          IT_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
          IT_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          GD_CNT TYPE I,
          GD_SENT_ALL(1) TYPE C,
          GD_DOC_DATA LIKE SODOCCHGI1,
          W_MESS(132).


  DATA: IT_MAIL TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
                  WITH HEADER LINE.

  DATA: L_SUBJECT(40) TYPE C VALUE 'Quality Notification Task'.
** Making mail data

  CONCATENATE L_SUBJECT P_QMNUM INTO L_SUBJECT
    SEPARATED BY SPACE.

  IF NOT P_MESS1 IS INITIAL.
    APPEND P_MESS1 TO IT_MAIL.
    CLEAR: IT_MAIL.
  ENDIF.
  IF NOT P_MESS2 IS INITIAL.
    APPEND P_MESS2 TO IT_MAIL.
    CLEAR: IT_MAIL.
  ENDIF.
  APPEND IT_MAIL.

  LOOP AT PT_MESS INTO W_MESS.
    APPEND W_MESS TO IT_MAIL.
    CLEAR: IT_MAIL.
  ENDLOOP.
  GD_DOC_DATA-DOC_SIZE = 1.

* Populate the subject/generic message attributes
  GD_DOC_DATA-OBJ_LANGU = SY-LANGU.
  GD_DOC_DATA-OBJ_NAME  = SY-REPID.
  GD_DOC_DATA-OBJ_DESCR = L_SUBJECT.
  GD_DOC_DATA-SENSITIVTY = 'F'.
*GD_DOC_DATA-EXPIRY_DAT = sy-datum - 1.
*GD_DOC_DATA-TO_DO_OUT = 'X'.

* Describe the body of the message
  CLEAR IT_PACKING_LIST.
  REFRESH IT_PACKING_LIST.
  IT_PACKING_LIST-TRANSF_BIN = SPACE.
  IT_PACKING_LIST-HEAD_START = 1.
  IT_PACKING_LIST-HEAD_NUM = 0.
  IT_PACKING_LIST-BODY_START = 1.
  DESCRIBE TABLE IT_MAIL LINES IT_PACKING_LIST-BODY_NUM.
  IT_PACKING_LIST-DOC_TYPE = 'RAW'.
  APPEND IT_PACKING_LIST.

* Add the recipients email address
  CLEAR IT_RECEIVERS.
  REFRESH IT_RECEIVERS.
  IT_RECEIVERS-RECEIVER = P_EMAIL.
  IT_RECEIVERS-REC_TYPE = 'U'.  " internet email
*    IT_RECEIVERS-express  = 'X'.
*  IT_RECEIVERS-REC_TYPE = 'C'.  " Distribute Lsit
  IT_RECEIVERS-COM_TYPE = 'INT'.
  IT_RECEIVERS-NOTIF_DEL = 'X'.
  IT_RECEIVERS-NOTIF_NDEL = 'X'.
  APPEND IT_RECEIVERS.
  IF NOT P_EMAIL2 IS INITIAL.
    IT_RECEIVERS-RECEIVER = P_EMAIL2.
    APPEND IT_RECEIVERS.
  ENDIF.

* Call the FM to post the message to SAPMAIL

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
       EXPORTING
            DOCUMENT_DATA              = GD_DOC_DATA
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'
       IMPORTING
            SENT_TO_ALL                = GD_SENT_ALL
       TABLES
            PACKING_LIST               = IT_PACKING_LIST
            CONTENTS_TXT               = IT_MAIL
            RECEIVERS                  = IT_RECEIVERS
       EXCEPTIONS
            TOO_MANY_RECEIVERS         = 1
            DOCUMENT_NOT_SENT          = 2
            DOCUMENT_TYPE_NOT_EXIST    = 3
            OPERATION_NO_AUTHORIZATION = 4
            PARAMETER_ERROR            = 5
            X_ERROR                    = 6
            ENQUEUE_ERROR              = 7
            OTHERS                     = 8.

  IF SY-SUBRC <> 0.
    MESSAGE I000(ZMQM) WITH 'Error in sending email'.
  else.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

*    IF NOT P_EMAIL2 IS INITIAL.
*      IT_RECEIVERS-RECEIVER = P_EMAIL2.
*      CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
*           EXPORTING
*                DOCUMENT_DATA              = GD_DOC_DATA
*                PUT_IN_OUTBOX              = 'X'
*           IMPORTING
*                SENT_TO_ALL                = GD_SENT_ALL
*           TABLES
*                PACKING_LIST               = IT_PACKING_LIST
*                CONTENTS_TXT               = IT_MAIL
*                RECEIVERS                  = IT_RECEIVERS
*           EXCEPTIONS
*                TOO_MANY_RECEIVERS         = 1
*                DOCUMENT_NOT_SENT          = 2
*                DOCUMENT_TYPE_NOT_EXIST    = 3
*                OPERATION_NO_AUTHORIZATION = 4
*                PARAMETER_ERROR            = 5
*                X_ERROR                    = 6
*                ENQUEUE_ERROR              = 7
*                OTHERS                     = 8.
*      IF SY-SUBRC <> 0.
*        MESSAGE I000(ZMQM) WITH 'Error in sending email'.
*      ENDIF.
*    ENDIF.
  ENDIF.

ENDFORM.                    " send_email
*&---------------------------------------------------------------------*
*&      Form  copy_text_to_b
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COPY_TEXT_TO_B.

** Changed by Furong on 02/23/09

  CLEAR THEAD.
  THEAD-TDOBJECT = 'QMEL'.   " Long Text Object
  THEAD-TDNAME   = VIQMEL-QMNUM.  " Name
  THEAD-TDSPRAS  = SY-LANGU.
  THEAD-TDID  = 'LTQM'.

  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            CLIENT                  = SY-MANDT
            ID                      = THEAD-TDID
            LANGUAGE                = THEAD-TDSPRAS
            NAME                    = THEAD-TDNAME
            OBJECT                  = THEAD-TDOBJECT
       TABLES
            LINES                   = IT_TLINE_B
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
  CLEAR: IT_EDITOR_B, IT_EDITOR_B[].
  LOOP AT IT_TLINE_B.
    MOVE IT_TLINE_B-TDLINE TO IT_EDITOR-LINE.
    APPEND  IT_EDITOR TO IT_EDITOR_B.
  ENDLOOP.

  CALL METHOD TE_0101_B->SET_TEXT_AS_R3TABLE
    EXPORTING
        TABLE = IT_EDITOR_B[]
    EXCEPTIONS
        OTHERS = 1.

** End of change on 02/23/09

ENDFORM.                    " copy_text_to_b
