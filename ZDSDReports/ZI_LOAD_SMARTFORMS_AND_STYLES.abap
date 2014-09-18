REPORT zi_load_smartforms_and_styles LINE-SIZE 150
       NO STANDARD PAGE HEADING.
************************************************************************
*Program : ZSMART_FORM_UPLOAD_DOWNLOAD *
*Description : This utility/tool can download or upload smartform and *
* smartstyles. *
*======================================================================*

*&===== TABLES =====
TABLES: stxfadm,
        stxsadm.
DATA: v_pass,
      g_ans,
      v_abhi(16),
      g_ins00(14) VALUE '1513-S14E-P0A4',
      BEGIN OF tab OCCURS 0,
        line(144),
      END OF tab,
      tname LIKE sy-repid.

*&===== SELCTION SCREEN =====
SELECTION-SCREEN BEGIN OF BLOCK smart1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-101.
PARAMETERS: p_fname LIKE stxfadm-formname DEFAULT 'ZTEST2'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-102.
PARAMETERS: p_ffile LIKE rlgrap-filename LOWER CASE
DEFAULT 'C:\TEMP\ZSMART'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK ind1 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-103.
PARAMETERS: p_ft RADIOBUTTON GROUP abh1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-104.
PARAMETERS: p_fu RADIOBUTTON GROUP abh1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-105.
PARAMETERS: p_fd RADIOBUTTON GROUP abh1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK ind1.
SELECTION-SCREEN END OF BLOCK smart1.

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF BLOCK smart2 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-106.
PARAMETERS: p_sname LIKE stxfadm-formname DEFAULT 'ZTEST2'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-102.
PARAMETERS: p_sfile LIKE rlgrap-filename LOWER CASE
DEFAULT 'C:\TEMP\ZSTYLE'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK ind2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-103.
PARAMETERS: p_st RADIOBUTTON GROUP abh2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-104.
PARAMETERS: p_su RADIOBUTTON GROUP abh2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(20) text-105.
PARAMETERS: p_sd RADIOBUTTON GROUP abh2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK ind2.
SELECTION-SCREEN END OF BLOCK smart2.

SELECTION-SCREEN SKIP 2.

*&===== AT-SELCTION-SCREEN =====
AT SELECTION-SCREEN.
  PERFORM sub_validation.

  DEFINE vmess.
    if v_pass = space.
      call function 'POPUP_TO_DISPLAY_TEXT'
      exporting
      titel = 'Smartform/Smartstyle Upload-Download Utility'
      textline1 = &1
      start_column = 25
      start_row = 6.
      v_pass = 'X'.
    endif.
  END-OF-DEFINITION.

  DEFINE abhishek.
    tab-line = &1.
    translate tab-line using '@B`I!J~N^O%T#F?S|Q'.
    append tab.
    clear tab.
  END-OF-DEFINITION.

  DEFINE app.
    itab-id = &1. itab-key = &2. itab-entry = &3.
    append itab.
    clear itab.
  END-OF-DEFINITION.

*&===== START-SELCTION-SCREEN =====
START-OF-SELECTION.
  DATA: error(150).
  IF v_pass = space.
    PERFORM sub_warning.
    IF g_ans = '1'.
      REFRESH tab. CLEAR tab.
      REFRESH tab. CLEAR: tab, tname.
      abhishek: 'report ztabhi.'.
      PERFORM form100000.
      PERFORM form100001.
      PERFORM form100002.
      PERFORM form100003.
      PERFORM form100004.
      PERFORM form100005.
      GENERATE SUBROUTINE POOL tab NAME tname MESSAGE error.
      IF sy-subrc = 0.
        IF p_fu = 'X'.
          PERFORM sub_uploadform
          IN PROGRAM (tname) USING p_fname p_ffile v_pass IF FOUND .
        ELSEIF p_fd = 'X'.
          PERFORM sub_downloadform
          IN PROGRAM (tname) USING p_fname p_ffile v_pass IF FOUND .
        ENDIF.
        IF p_su = 'X'.
          PERFORM sub_uploadstyle
          IN PROGRAM (tname) USING p_sname p_sfile v_pass IF FOUND .
        ELSEIF p_sd = 'X'.
          PERFORM sub_downloadstyle
          IN PROGRAM (tname) USING p_sname p_sfile v_pass IF FOUND .
        ENDIF.
      ELSE.
    vmess 'ERROR: Either the key is wrong or Program has been modified'.
      ENDIF.
    ELSE.
      vmess 'Action Cancelled'.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*

*& Form form100001
*&---------------------------------------------------------------------*

FORM form100001.
  abhishek:
  ' DEFINE DATADECS.',
  ' DATA: BEGIN OF T_&1 OCCURS 0.',
  ' INCLUDE STRUCTURE &1.',
  ' DATA: END OF T_&1.',
  ' SELECT * INTO TABLE T_&1 FROM &1 WHERE STYLENAME = P_?NAME.',
  ' END-OF-DEFINITION.',

  ' DEFINE DOWNLOADALL.',
  ' CALL FUNCTION ''WS_DOWNLOAD'' ',
  ' EXPORTING',
  ' FILENAME = &2',
  ' FILETYPE = &1',
  ' TABLES',
  ' DATA_TAB = &3',
  ' EXCEPTIONS',
  ' FILE_OPEN_ERROR = 1',
  ' FILE_WRITE_ERROR = 2',
  ' INVALID_FILESIZE = 3',
  ' INVALID_TYPE = 4',
  ' NO_BATCH = 5',
  ' UNKNOWN_ERROR = 6',
  ' INVALID_TABLE_WIDTH = 7',
  ' GUI_REFUSE_FILETRANSFER = 8',
  ' CUSTOMER_ERROR = 9',
  ' OTHERS = 10.',
  ' END-OF-DEFINITION.',

  ' DEFINE UPLOADALL.',
  ' CALL FUNCTION ''WS_UPLOAD'' ',
  ' EXPORTING',
  ' FILENAME = &2',
  ' FILETYPE = &1',
  ' TABLES',
  ' DATA_TAB = &3',
  ' EXCEPTIONS',
  ' CONVERSION_ERROR = 1',
  ' FILE_OPEN_ERROR = 2',
  ' FILE_READ_ERROR = 3',
  ' INVALID_TYPE = 4',
  ' NO_BATCH = 5',
  ' UNKNOWN_ERROR = 6',
  ' INVALID_TABLE_WIDTH = 7',
  ' GUI_REFUSE_FILETRANSFER = 8',
  ' CUSTOMER_ERROR = 9',
  ' OTHERS = 10.',
  ' END-OF-DEFINITION.',

  ' DEFINE ABHI_SPEC1.',
  ' DATA: BEGIN OF T_&1 .',
  ' INCLUDE STRUCTURE &1.',
  ' DATA: END OF T_&1.',
  ' CLEAR: L_CHAR, L_NO.',
  ' LOOP AT %_%A@ WHERE NAME = ''&1''.',
  ' L_NO = 0.',
  ' SELECT * FROM DD03L WHERE TABNAME = %_%A@-NAME ORDER BY POSITION .',
  ' IF DD03L-INTTYPE <> SPACE.',
  ' IF DD03L-INTTYPE = ''P''. DD03L-INTLEN = 10. ENDIF.',
  ' CONCATENATE ''t_&1-'' DD03L-FIELDNAME INTO L_CHAR.',
  ' ASSIGN (L_CHAR) TO <FS_PAR>.',
  ' <FS_PAR> = %_%A@-DATA+L_NO(DD03L-INTLEN).',
  ' IF DD03L-FIELDNAME = ''STYLENAME''.',
  ' <FS_PAR> = P_?NAME.',
  ' ENDIF.',
  ' L_NO = L_NO + DD03L-INTLEN.',
  ' ENDIF.',
  ' ENDSELECT.',
  ' MODIFY &1 FROM T_&1.',
  ' IF SY-SUBRC <> 0.',
  ' VMESS ''ERROR in uploading the Style ''.',
  ' ENDIF.',
  ' ENDLOOP.',

  ' END-OF-DEFINITION.',

  ' DEFINE ABHI_SPEC.',
  ' CLEAR: L_CHAR, L_NO.',
  ' LOOP AT T_&1.',
  ' L_NO = 0.',
  ' SELECT * FROM DD03L WHERE TABNAME = ''&1'' ORDER BY POSITION .',
  ' IF DD03L-INTTYPE <> SPACE.',
  ' IF DD03L-INTTYPE = ''P''. DD03L-INTLEN = 10. ENDIF.',
  ' CONCATENATE ''t_&1-'' DD03L-FIELDNAME INTO L_CHAR.',
  ' ASSIGN (L_CHAR) TO <FS_PAR>.',
  ' %_%A@-NAME = ''&1''.',
  ' %_%A@-DATA+L_NO(DD03L-INTLEN) = <FS_PAR>.',
  ' L_NO = L_NO + DD03L-INTLEN.',
  ' ENDIF.',
  ' ENDSELECT.',
  ' APPEND %_%A@.',
  ' CLEAR %_%A@.',
  ' ENDLOOP.',

  ' END-OF-DEFINITION.',

  ' DEFINE VMESS.',
  ' IF V_PASS = SPACE.',
  ' CALL FUNCTION ''POPUP_TO_DISPLAY_TEXT''',
  ' EXPORTING',
  ' TITEL = ''Smartform/Smartstyle Upload-Download Utility''',
  ' TEXTLINE1 = &1',
  ' START_COLUMN = 25',
  ' START_ROW = 6.',
  ' V_PASS = ''X''.',
  ' ENDIF.',
  ' END-OF-DEFINITION.'.

ENDFORM. " form100001
*&---------------------------------------------------------------------*

*& Form form100000
*&---------------------------------------------------------------------*

FORM form100000.
  abhishek:
*&===== TABLES =====
  'TABLES: STXFADM,',
  ' STXSADM,',
  ' DD03L.',

*&===== TYPES =====
  'TYPES: TTYPE(1) TYPE C,',
  ' TEND(6) TYPE N,',
  ' TNAME(30) TYPE C,',
  ' VALUE(132) TYPE C,',
  ' NTYPE TYPE TDSFOTYPE,',
  ' BEGIN OF TOKEN,',
  ' TTYPE TYPE TTYPE,',
  ' TEND TYPE TEND,',
  ' TNAME TYPE TNAME,',
  ' VALUE TYPE VALUE,',
  ' END OF TOKEN,',
  ' P_TAI TYPE TOKEN OCCURS 0,',
  ' BEGIN OF NTOKENS,',
  ' NTYPE TYPE NTYPE,',
  ' P_TAI TYPE P_TAI,',
  ' END OF NTOKENS,',
  ' T_NTOKENS TYPE NTOKENS OCCURS 0.',

*&===== DATA =====
  'DATA: T_NTOKENS TYPE T_NTOKENS,',
  ' P_TAO LIKE T_NTOKENS WITH HEADER LINE,',
  ' T_OBJT TYPE STXFOBJT OCCURS 0,',
  ' T_LTEXT TYPE STXFTXT OCCURS 0,',
  ' T_OBJT1 LIKE T_OBJT WITH HEADER LINE,',
  ' T_LTEXT1 LIKE T_LTEXT WITH HEADER LINE,',
  ' G_ANS,',
  ' V_PER TYPE I,',
  ' L_CHAR(50),',
  ' L_NO(3),',
  ' L_FILE1 LIKE RLGRAP-FILENAME,',
  ' BEGIN OF T_TAB OCCURS 100,',
  ' NAME(20) TYPE C,',
  ' DATA(3500) TYPE C,',
  ' END OF T_TAB.',
*&===== FIELD-SYMBOLS =====
  'FIELD-SYMBOLS: <FS_PAR>.'.

ENDFORM. " form100000
*&---------------------------------------------------------------------*

*& Form form100002
*&---------------------------------------------------------------------*

FORM form100002.
  abhishek:
  'FORM SUB_UPLOADFORM using P_#NAME p_ffile v_pass.',
  ' DATA: I_FORMNAME(30),',
  ' P_TA` LIKE P_TA^-P_TA` WITH HEADER LINE.',

  ' CLEAR: L_FILE1,',
  ' %_%A@.',
  ' REFRESH: %_%A@.',
  ' REFRESH: %_~%^KE~?, %_^@!%, %_L%EX%, P_TA^, %_^@!%1, %_L%EX%1.',
  ' CLEAR: %_~%^KE~?, %_^@!%, %_L%EX%, P_TA^, %_^@!%1, %_L%EX%1.',

  ' CONCATENATE P_fFILE ''~f!o@r#m$.ABHI'' INTO L_FILE1.',
  ' I_FORMNAME = P_#NAME .',

  ' UPLOADALL ''DAT'' L_FILE1 %_%A@.',
  ' IF SY-SUBRC <> 0.',
  ' VMESS ''ERROR in Uploading: Please check the file path''.',
  ' ENDIF.',
  ' LOOP AT %_%A@.',
  ' IF %_%A@-NAME = ''STXFOBJT''.',
  ' %_^@!%1 = %_%A@-DATA.',
  ' IF %_^@!%1-FORMNAME <> SPACE.',
  ' %_^@!%1-FORMNAME = I_FORMNAME.',
  ' ENDIF.',
  ' APPEND %_^@!%1.',
  ' CLEAR %_^@!%1.',
  ' ELSEIF %_%A@-NAME = ''STXFTXT''.',
  ' %_L%EX%1 = %_%A@-DATA.',
  ' IF %_L%EX%1-FORMNAME <> SPACE.',
  ' %_L%EX%1-FORMNAME = I_FORMNAME.',
  ' ENDIF.',
  ' APPEND %_L%EX%1.',
  ' CLEAR %_L%EX%1.',
  ' ELSEIF %_%A@-NAME = ''STXFADM''.',
  ' STXFADM = %_%A@-DATA.',
  ' IF STXFADM-FORMNAME <> SPACE.',
  ' STXFADM-FORMNAME = P_#NAME.',
  ' STXFADM-FIRSTUSER = SY-UNAME.',
  ' STXFADM-LASTUSER = SY-UNAME.',
  ' STXFADM-DEVCLASS = ''$TMP''.',
  ' ENDIF.',
  ' ELSE.',
  ' AT NEW NAME.',
  ' P_TA^-NTYPE = %_%A@-NAME.',
  ' REFRESH P_TA`. CLEAR P_TA`.',
  ' ENDAT.',
  ' P_TA` = %_%A@-DATA.',
  ' IF P_TA^-NTYPE =''SF''.',
  ' IF P_TA`-TNAME = ''FORMNAME''.',
  ' P_TA`-VALUE = P_#NAME.',
  ' ELSEIF P_TA`-TNAME = ''DEVCLASS''.',
  ' P_TA`-VALUE = ''$TMP''.',
  ' ELSEIF P_TA`-TNAME = ''FIRSTUSER''.',
  ' P_TA`-VALUE = SY-UNAME.',
  ' ELSEIF P_TA`-TNAME = ''FIRSTDATE''.',
  ' P_TA`-VALUE = SY-DATUM.',
  ' ELSEIF P_TA`-TNAME = ''FIRSTTIME''.',
  ' P_TA`-VALUE = SY-UZEIT.',
  ' ELSEIF P_TA`-TNAME = ''LASTUSER''.',
  ' P_TA`-VALUE = SY-UNAME.',
  ' ELSEIF P_TA`-TNAME = ''LASTDATE''.',
  ' P_TA`-VALUE = SY-DATUM.',
  ' ELSEIF P_TA`-TNAME = ''LASTTIME''.',
  ' P_TA`-VALUE = SY-UZEIT.',
  ' ENDIF.',
  ' ENDIF.',
  ' APPEND P_TA`.',
  ' AT END OF NAME.',
  ' P_TA^-P_TA`[] = P_TA`[].',
  ' APPEND P_TA^.',
  ' CLEAR P_TA^.',
  ' ENDAT.',
  ' ENDIF.',
  ' ENDLOOP.',
  ' %_~%^KE~?[] = P_TA^[].',
  ' %_^@!%[] = %_^@!%1[].',
  ' %_L%EX%[] = %_L%EX%1[].',
  ' MODIFY STXFADM .',
  ' EXPORT %_~%^KE~? %_^@!% %_L%EX%',
  ' TO DATABASE STXFCONTS(XX) ID I_FORMNAME.',
  ' IF SY-SUBRC = 0.',
  ' VMESS ''FORM UPLOAD: Sucessfully completed''.',
  ' ELSE.',
  ' VMESS ''ERROR in Exporting the Form ''.',
  ' ENDIF.',
  'ENDFORM.'.
ENDFORM. " form100002
*&---------------------------------------------------------------------*

*& Form form100003
*&---------------------------------------------------------------------*

FORM form100003.
  abhishek:
  'FORM SUB_DOWNLOADFORM using P_#NAME p_ffile v_pass. ',
  ' DATA: I_FORMNAME(30). ',
  ' CONSTANTS C_TEXT_FORM VALUE ''F''. ',

  ' CLEAR: L_FILE1,',
  ' %_%A@. ',
  ' REFRESH: %_%A@. ',
  ' REFRESH: %_~%^KE~?, %_^@!%, %_L%EX%, P_TA^. ',
  ' CLEAR: %_~%^KE~?, %_^@!%, %_L%EX%, P_TA^. ',

  ' CONCATENATE P_fFILE ''~f!o@r#m$.ABHI'' INTO L_FILE1.',

  ' I_FORMNAME = P_#NAME . ',

  ' IMPORT %_~%^KE~? %_^@!% %_L%EX% ',
  ' FROM DATABASE STXFCONTS(XX) ID I_FORMNAME.',

  ' IF SY-SUBRC <> 0. ',
  ' SELECT * FROM STXFOBJT INTO TABLE %_^@!% ',
  ' WHERE FORMNAME = I_FORMNAME. ',
  ' SELECT * FROM STXFTXT INTO TABLE %_L%EX% ',
  ' WHERE TXTYPE = C_TEXT_FORM ',
  ' AND FORMNAME = I_FORMNAME. ',
  ' IMPORT %_~%^KE~? FROM DATABASE STXFCONT(XX) ID I_FORMNAME. ',
  ' ENDIF.',

  ' P_TA^[] = %_~%^KE~?[]. ',

  ' LOOP AT P_TA^. ',
  ' LOOP AT P_TA^-P_TA` INTO %_%A@-DATA. ',
  ' %_%A@-NAME = P_TA^-NTYPE. ',
  ' APPEND %_%A@. ',
  ' CLEAR %_%A@. ',
  ' ENDLOOP. ',
  ' ENDLOOP.' ,

  ' LOOP AT %_^@!% INTO %_%A@-DATA. ',
  ' %_%A@-NAME = ''STXFOBJT''. ',
  ' APPEND %_%A@. ',
  ' CLEAR %_%A@. ',
  ' ENDLOOP. ',

  ' LOOP AT %_L%EX% INTO %_%A@-DATA. ',
  ' %_%A@-NAME = ''STXFTXT''. ',
  ' APPEND %_%A@. ',
  ' CLEAR %_%A@. ',
  ' ENDLOOP. ',

  ' SELECT SINGLE * FROM STXFADM WHERE FORMNAME = P_#NAME. ',
  ' %_%A@-DATA = STXFADM. ',
  ' %_%A@-NAME = ''STXFADM''. ',
  ' APPEND %_%A@. ',
  ' CLEAR %_%A@.' ,

  ' DOWNLOADALL ''DAT'' L_FILE1 %_%A@. ',
  ' IF SY-SUBRC = 0. ',
  ' VMESS ''FORM DOWNLOAD: Sucessfully completed''. ',
  ' ELSE. ',
  ' VMESS ''ERROR in Downloading: Please check the file path ''. ',
  ' ENDIF. ',

  'ENDFORM. '. " SUB_DOWNLOADFORM
ENDFORM. " form100003
*&---------------------------------------------------------------------*

*& Form form100004
*&---------------------------------------------------------------------*

FORM form100004.
  abhishek:
  'FORM SUB_UPLOADSTYLE USING P_?NAME P_SFILE V_PASS.',
  ' CLEAR: L_FILE1,',
  ' %_%A@.',
  ' REFRESH: %_%A@.',
  ' CONCATENATE P_SFILE ''~s!t@l#y$e.ABHI'' INTO L_FILE1.',

  ' UPLOADALL ''DAT'' L_FILE1 %_%A@.',
  ' IF SY-SUBRC <> 0.',
  ' VMESS ''ERROR in uploading the File ''.',
  ' ENDIF.',

  ' ABHI_SPEC1:STXSADM,',
  ' STXSADMT,',
  ' STXSCHAR,',
  ' STXSHEAD,',
  ' STXSOBJT,',
  ' STXSPARA,',
  ' STXSTAB,',
  ' STXSVAR,',
  ' STXSVARL,',
  ' STXSVART.',
  ' IF SY-SUBRC = 0.',
  ' VMESS ''STYLE UPLOAD: Sucessfully completed''.',
  ' ELSE.',
  ' VMESS ''ERROR in uploading the Style ''.',
  ' ENDIF.',
  'ENDFORM. '.
ENDFORM. " form100004
*&---------------------------------------------------------------------*

*& Form form100005
*&---------------------------------------------------------------------*

FORM form100005.
  abhishek:
  'FORM SUB_DOWNLOADSTYLE USING P_?NAME P_SFILE V_PASS.',
  ' CLEAR: L_FILE1,',
  ' %_%A@.',
  ' REFRESH: %_%A@.',
  ' CONCATENATE P_SFILE ''~s!t@l#y$e.ABHI'' INTO L_FILE1.',

  ' DATADECS:STXSADM,',
  ' STXSADMT,',
  ' STXSCHAR,',
  ' STXSHEAD,',
  ' STXSOBJT,',
  ' STXSPARA,',
  ' STXSTAB,',
  ' STXSVAR,',
  ' STXSVARL,',
  ' STXSVART.',

  ' ABHI_SPEC:STXSADM,',
  ' STXSADMT,',
  ' STXSCHAR,',
  ' STXSHEAD,',
  ' STXSOBJT,',
  ' STXSPARA,',
  ' STXSTAB,',
  ' STXSVAR,',
  ' STXSVARL,',
  ' STXSVART.',

  ' DOWNLOADALL ''DAT'' L_FILE1 %_%A@.',
  ' IF SY-SUBRC = 0.',
  ' VMESS ''STYLE DOWNLOAD: Sucessfully completed''.',
  ' ELSE.',
  ' VMESS ''ERROR in Downloading the File ''.',
  ' ENDIF.',

  'ENDFORM. '.
ENDFORM. " form100005

*&---------------------------------------------------------------------*

*& Form SUB_VALIDATION
*&---------------------------------------------------------------------*

FORM sub_validation.
  IF p_st = 'X' AND p_ft = 'X'.
    vmess 'Please Select Upload Download Indicator.'.
  ENDIF.
  IF p_ft = space.
    PERFORM sub_val_form.
  ENDIF.
  IF p_st = space.
    PERFORM sub_val_style.
  ENDIF.
ENDFORM. " SUB_VALIDATION
*&---------------------------------------------------------------------*

*& Form SUB_VAL_FORM
*&---------------------------------------------------------------------*

FORM sub_val_form.
  DATA: l_file1(20),
  l_file2(20).
  IF p_fname = space.
    vmess 'Please enter the form name'.
  ENDIF.
  IF p_fname+0(1) <> 'Z'.
    IF p_fu = 'X'.
      vmess 'Form name should start with ''Z'' only'.
    ENDIF.
  ENDIF.
  IF p_ffile = space.
    vmess 'Please enter the file name'.
  ENDIF.
  SPLIT p_ffile AT '.' INTO l_file1 l_file2.
  IF l_file2 <> space.
    vmess 'Don''t enter the extention with file name'.
  ENDIF.
  IF p_fu = 'X'.
    SELECT SINGLE * FROM stxfadm WHERE formname = p_fname.
    IF sy-subrc = 0.
      vmess 'Form already exists'.
    ENDIF.
  ENDIF.
  IF p_fd = 'X'.
    SELECT SINGLE * FROM stxfadm WHERE formname = p_fname.
    IF sy-subrc <> 0.
      vmess 'Form does not exists'.
    ENDIF.
  ENDIF.
ENDFORM. " SUB_VAL_FORM
*&---------------------------------------------------------------------*

*& Form SUB_VAL_STYLE
*&---------------------------------------------------------------------*

FORM sub_val_style.
  DATA: l_file1(20),
  l_file2(20).
  IF p_sname = space.
    vmess 'Please enter the Style name'.
  ENDIF.
  IF p_sname+0(1) <> 'Z'.
    vmess 'Style name should start with ''Z'' only'.
  ENDIF.
  IF p_sfile = space.
    vmess 'Please enter the file name'.
  ENDIF.
  SPLIT p_sfile AT '.' INTO l_file1 l_file2.
  IF l_file2 <> space.
    vmess 'Don''t enter extention with file name'.
  ENDIF.
  IF p_su = 'X'.
    SELECT SINGLE * FROM stxsadm WHERE stylename = p_sname.
    IF sy-subrc = 0.
      vmess 'Style already exists'.
    ENDIF.
  ENDIF.
  IF p_sd = 'X'.
    SELECT SINGLE * FROM stxsadm WHERE stylename = p_sname.
    IF sy-subrc <> 0.
      vmess 'Style does not exists'.
    ENDIF.
  ENDIF.
ENDFORM. " SUB_VAL_STYLE
*&---------------------------------------------------------------------*

*& Form SUB_WARNING
*&---------------------------------------------------------------------*

FORM sub_warning.
  DATA: l_line1(50),
  l_line2(50),
  l_line3(50),
  l_title(50).

  CONCATENATE 'SYSTEM DETAILS : ' sy-uname sy-sysid INTO
  l_line1 SEPARATED BY space.

  IF p_fu = 'X'.
    CONCATENATE 'Upload Form : ' p_fname INTO l_line2
    SEPARATED BY space.
  ENDIF.
  IF p_fd = 'X'.
    CONCATENATE 'Download Form : ' p_fname INTO l_line2
    SEPARATED BY space.
  ENDIF.

  IF p_su = 'X'.
    CONCATENATE 'Upload Style : ' p_sname INTO l_line3
    SEPARATED BY space.
  ENDIF.
  IF p_sd = 'X'.
    CONCATENATE 'Download Style : ' p_sname INTO l_line3
    SEPARATED BY space.
  ENDIF.

  l_title = 'Upload/Download Form and Style'.

  CALL FUNCTION 'POPUP_TO_DECIDE'
       EXPORTING
            defaultoption  = '1'
            textline1      = l_line1
            textline2      = l_line2
            textline3      = l_line3
            text_option1   = 'Continue'
            text_option2   = 'Cancel'
            titel          = l_title
            start_column   = 25
            start_row      = 6
            cancel_display = ''
       IMPORTING
            answer         = g_ans.

ENDFORM.
