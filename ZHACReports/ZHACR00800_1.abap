*&---------------------------------------------------------------------*
*& Report  ZHACR00800_1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZHACR00800_1  NO STANDARD PAGE HEADING  LINE-SIZE 90
                      MESSAGE-ID EU.

******************  TABLE ### # ## PARAMETER  ********************
TABLES : TRDIR, TRDIRT, ARCH_OBJ, ARCH_DEF, ZITTACV001.

SELECTION-SCREEN BEGIN OF BLOCK BR1 WITH FRAME TITLE TEXT-001.

* ### CONFIG TABLE ## ### ##
PARAMETERS : P_OBJ(10) OBLIGATORY MEMORY ID OBT,
             P_TAB(30) OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BR1.

SELECTION-SCREEN BEGIN OF BLOCK BR2 WITH FRAME TITLE TEXT-002.
PARAMETERS : P_TOP(30)   OBLIGATORY,
             P_PRO(30)   OBLIGATORY,
             P_DEL(30),
             P_RLD(30),
             P_REA(30),
             P_TEXT(50).
SELECTION-SCREEN SKIP 1.
PARAMETERS : P_PACK(10)  DEFAULT '10000'.

SELECTION-SCREEN END OF BLOCK BR2.

*****************************  DATA ##  ****************************
DATA : INDEX1(72),
       INDEX2(72),
       INDEX3(72).
* ###### ### ##
DATA : BEGIN OF ITAB OCCURS 10,
         DATA(72),
       END OF ITAB.

DATA : BEGIN OF GT_DD03L OCCURS 0,
         TABNAME LIKE DD03L-TABNAME,
         FIELDNAME LIKE DD03L-FIELDNAME,
         POSITION LIKE DD03L-POSITION,
       END OF GT_DD03L.
DATA: L_PARATAB TYPE ZLTEXT.
DATA :  PARATAB LIKE  ZITTACV002    OCCURS 0 WITH HEADER LINE.

DATA : TEXTTOP(100),
       TEXTACV(100),
       TEXTDEL(100),
       TEXTRLD(100),
       TEXTREA(100).
DATA : OBJNAME(12).
DATA: G_ERROR(255).
DATA: L_LIST_TOTALLINE(15).

*****************************  MACRO ##  ****************************
DEFINE MAKE_GEN_ALL_PROGRAM.
* &1 : Create archive source in Perform
* &2 : Program code
* &3 : Program name
* &4 : Program type  1 : Report,  I : Include
  CHECK NOT &2  IS INITIAL.
  PERFORM &1.
  COMMIT WORK.
  INSERT REPORT &2    FROM ITAB.
  IF SY-SUBRC = 0.
    COMMIT WORK.

    CALL FUNCTION 'RS_CORR_INSERT'
         EXPORTING
              OBJECT              = &2
              OBJECT_CLASS        = 'ABAP'
*              DEVCLASS            = 'ZITACV'
              DEVCLASS            = 'ZHAC'
              KORRNUM             = ''
         EXCEPTIONS
              CANCELLED           = 1
              PERMISSION_FAILURE  = 2
              UNKNOWN_OBJECTCLASS = 3
              OTHERS              = 4.
    IF SY-SUBRC <> 0.
      WRITE : / 'Dev Class fail',   &2.
    ENDIF.

    PERFORM SET_ATTRIBUTE_PROGRAM USING  &2 &3 &4.
  ELSE.
    WRITE : / 'program', &2 ,'create fail', 'SY-SUBRC = ', SY-SUBRC.
  ENDIF.

END-OF-DEFINITION.

*************************INITIALIZATION*************************
INITIALIZATION.
***  GET PARAMETER ID 'OBT' FIELD P_OBJ.
***
**** #### ## ##### ####
***  SELECT SINGLE * FROM ZITTACV001
***    WHERE OBJECT = P_OBJ.
***  IF SY-SUBRC  =  0.
***    P_TAB  = ZITTACV001-MTABLE.
***    P_TEXT = ZITTACV001-OBJTEXT.
**** ###### AOBJ  ## ### # ## # #######
**** ######
***    SELECT SINGLE * FROM ARCH_OBJ
***           WHERE OBJECT = P_OBJ.
***    IF SY-SUBRC = 0 AND NOT ( ARCH_OBJ-REORGA_PRG IS INITIAL ).
***      P_TOP      = ARCH_OBJ-REORGA_PRG(9).
***      P_TOP+9(3) = 'TOP'.
***      P_PRO(30)  = ARCH_OBJ-REORGA_PRG.
***      P_DEL(30)  = ARCH_OBJ-DELETE_PRG.
***      P_RLD(30)  = ARCH_OBJ-RETRIE_PRG.
**** ###### AOBJ ## ### ## ## ###### ####
**** FI : ZFIACR010W, 020W ...
**** CO : ZCOACR010W, 020W ...
***    ELSE.
***      DATA : L_PRGNAMELOW(30),
***             L_PRGNAMEHIGH(30),
***             L_SEQ(3) TYPE N,
***             L_NEW_PRGNAME(30).
***
***      CONCATENATE  'Z' ZITTACV001-APPLIC 'ACR001W'
***                   INTO L_PRGNAMELOW.
***      CONCATENATE  'Z' ZITTACV001-APPLIC 'ACR999W'
***                   INTO L_PRGNAMEHIGH.
***
***      SELECT * FROM TRDIR
***               WHERE NAME BETWEEN L_PRGNAMELOW   AND
***                                  L_PRGNAMEHIGH
***               ORDER BY NAME DESCENDING.
***        EXIT.
***      ENDSELECT.
***      IF SY-SUBRC = 0.
***        L_SEQ = TRDIR-NAME+6(3).
***        L_SEQ = L_SEQ + 10.
***        L_NEW_PRGNAME = L_PRGNAMELOW(6).
***        L_NEW_PRGNAME+6(3) = L_SEQ.
***      ELSE.
***        L_NEW_PRGNAME = L_PRGNAMELOW(6).
***        L_NEW_PRGNAME+6(3) = '010'.
***      ENDIF.
***      CONCATENATE L_NEW_PRGNAME 'TOP' INTO P_TOP.
***      CONCATENATE L_NEW_PRGNAME 'W' INTO P_PRO.
***      CONCATENATE L_NEW_PRGNAME 'D' INTO P_DEL.
***      CONCATENATE L_NEW_PRGNAME 'L' INTO P_RLD.
***    ENDIF.
***  ENDIF.

*********************START-OF-SELECTION*************************
START-OF-SELECTION.
****************************************************************

* #### ## Config #### #### ## ###
*****  PERFORM CHECK_INVALID_OBJECT.

* #### # ####
  CONCATENATE  '''' P_OBJ '''' INTO OBJNAME.

  CONCATENATE  'Include' P_TOP INTO TEXTTOP SEPARATED BY SPACE.
  CONCATENATE  'Archiving :' P_TAB '(Write)'
               INTO TEXTACV SEPARATED BY SPACE.
  CONCATENATE  'Archiving :' P_TAB '(Delete)'
               INTO TEXTDEL SEPARATED BY SPACE.
  CONCATENATE  'Archiving :' P_TAB '(Reload)'
               INTO TEXTRLD SEPARATED BY SPACE.
  CONCATENATE  'Archiving :' P_TAB '(Read)'
               INTO TEXTREA SEPARATED BY SPACE.

  SKIP 1.
  WRITE : / 'Table            : ', P_TAB(15),
          / 'Archiving Object : ', P_OBJ.
  SKIP 1.
  WRITE   / 'create program list (Archive/Delete/Reload/Read)'.
  ULINE.

* #### ## #### ## ##### ##
  PERFORM GET_ARCHIVE_PARAMETERS_FR_DB.

* ## TOP Include ##/GEN##
  MAKE_GEN_ALL_PROGRAM MAKE_TOP_INCLUDE
                       P_TOP
                       TEXTTOP
                       'I'.
  REFRESH ITAB.

  SELECT
    TABNAME
    FIELDNAME
    POSITION
    FROM DD03L
    INTO CORRESPONDING FIELDS OF TABLE GT_DD03L
   WHERE TABNAME EQ P_TAB
     AND KEYFLAG EQ 'X'
     AND POSITION NE '0001'.

* #### #### ##/GEN##
  MAKE_GEN_ALL_PROGRAM MAKE_ARCHIVE_PROGRAM
                       P_PRO
                       TEXTACV
                       '1'.
* ### ###### AOBJ # Update ##
  UPDATE ARCH_OBJ SET REORGA_PRG = P_PRO
                  WHERE OBJECT   = P_OBJ.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ENDIF.

  REFRESH ITAB.

* #### ##### TEXT Update ##
  PERFORM MODIFY_TEXT_FOR_ARCHIVE_PRO.

* ## #### ##/GEN##
  MAKE_GEN_ALL_PROGRAM MAKE_DELETE_PROGRAM
                       P_DEL
                       TEXTDEL
                       '1'.
* ### ###### AOBJ # Update ##
  UPDATE ARCH_OBJ SET DELETE_PRG = P_DEL
                  WHERE OBJECT   = P_OBJ.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ENDIF.

  REFRESH ITAB.
* RELOAD #### ##/GEN##
  MAKE_GEN_ALL_PROGRAM MAKE_RELOAD_PROGRAM
                       P_RLD
                       TEXTRLD
                       '1'.
* ### ###### AOBJ # Update ##
  UPDATE ARCH_OBJ SET RETRIE_PRG = P_RLD
                  WHERE OBJECT   = P_OBJ.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ENDIF.
  REFRESH ITAB.
* READ #### ##/GEN##
  MAKE_GEN_ALL_PROGRAM MAKE_READ_PROGRAM
                       P_REA
                       TEXTREA
                       '1'.

** ### ###### AOBJ # Update ##
*  UPDATE ARCH_OBJ SET REORGA_PRG = P_PRO
*                      RETRIE_PRG = P_RLD
*                      DELETE_PRG = P_DEL
*                  WHERE OBJECT   = P_OBJ.
  COMMIT WORK.
*&---------------------------------------------------------------------*
*&      Form  make_pro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PRONAME  text
*----------------------------------------------------------------------*
FORM MAKE_PRO USING    PROTEXT.
  IF PROTEXT = '/'.
    APPEND ITAB. CLEAR ITAB.
  ELSE.
    IF ITAB-DATA IS INITIAL.
      ITAB-DATA = PROTEXT.
    ELSE.
      CONCATENATE ITAB-DATA PROTEXT INTO ITAB-DATA SEPARATED BY SPACE.
    ENDIF.
  ENDIF.
ENDFORM.                    " make_pro

*&---------------------------------------------------------------------*
*&      Form  MAKE_TOP_INCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_TOP_INCLUDE.
  DATA TABNAME(80).
  CONCATENATE '''' P_TAB '''' INTO TABNAME.

  PERFORM MAKE_PRO USING :
'*&-------------------------------------------------------------*', '/',
'*& Include ', P_TOP, '/',
'*&-------------------------------------------------------------*', '/',
'*System name         : HMI SYSTEM', '/',
'*Sub system name     : ARCHIVE', '/',
'*Program name        :', TEXTTOP, '/',
'*Program descrition  : Generated automatically by the ZHACR00800', '/',
'*Created on   :  ', SY-DATUM, '         Created by   :', SY-UNAME, '/',
'*Changed on :                           Changed by    :', '/',
'*Changed descrition :', '/',
'*"-------------------------------------------------------------*', '/',
         '***** Tables used in delete program', '/',
         'TABLES: ', P_TAB, '.       " Object Table', '/',
         'TABLES: ARC_BUFFER, V_ARC_USR, ADMI_RUN, ADMI_FILES.',
         '/', '/',
         '* the handles for the archiv operations', '/',
         'DATA: HANDLE LIKE SY-TABIX,', '/',
         '      READ   LIKE SY-TABIX,', '/',
         '      WRITE  LIKE SY-TABIX.', '/',
         'DATA: COMMCNT   LIKE ARCH_USR-ARCH_COMIT,', '/',
         '      OBJCNT    LIKE ARCH_USR-ARCH_COMIT,', '/',
         '      INDEX     LIKE ARCH_USR-ARCH_INDEX,', '/',
         '      OBJECT_ID LIKE ARCH_IDX-OBJECT_ID,', '/',
         '      ARKEY     LIKE ARCH_IDX-ARCHIVEKEY,', '/',
         '      OFFSET    TYPE I,', '/',
         '      NUMBER_OF_RECORDS_READ TYPE I,', '/',
         '      DATA_CNT  TYPE I.', '/',
         'DATA  G_CURSOR     TYPE CURSOR.', '/',
         'DATA  G_PACKAGE    TYPE I VALUE  ', P_PACK, '.', '/',
         'DATA  DELETE_FROM_TABLE.', '/',
         'DATA DATA_OBJECT_ID LIKE ARCH_IDX_S-OBJ_ID.', '/', '/',
         '*****   Below the object that is wrong by few', '/',
         '* internal tables used in delete and reload program', '/',
         'DATA: T_ITAB  LIKE', P_TAB, 'OCCURS 0 WITH HEADER LINE,', '/',
         '      BUFFER  TYPE ARC_BUFFER,', '/',
         '      S_ITAB  LIKE', P_TAB, '.', '/',
         'DATA  ARC_STRUCT    LIKE ARC_BUFFER-RNAME VALUE', '/',
         '       ', TABNAME, '. "Layout', '/',
         'DATA  ARC_TABLE     LIKE ARC_BUFFER-RNAME VALUE', '/',
         '       ', TABNAME, '. "Table', '/'.
ENDFORM.                    " MAKE_TOP_INCLUDE

*&---------------------------------------------------------------------*
*&      Form  make_archive_program
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_ARCHIVE_PROGRAM.
  PERFORM MAKE_PRO USING :
'*&-------------------------------------------------------------*', '/',
'*& Report', P_PRO, '/',
'*&-------------------------------------------------------------*', '/',
'*System name         : HMI SYSTEM', '/',
'*Sub system name     : ARCHIVE', '/',
'*Program name        :', TEXTACV, '/',
'*Program descrition  : Generated automatically by the ZHACR00800', '/',
'*Created on   :  ', SY-DATUM, '         Created by   :', SY-UNAME, '/',
'*Changed on :                           Changed by    :', '/',
'*Changed descrition :', '/',
'*"-------------------------------------------------------------*', '/',
         'REPORT',  P_PRO, '.', '/', '/',
         '***** Include TOP ', '/',
         'INCLUDE', P_TOP, '.', '/', '/',
         '***** Selection screen.', '/',
        'SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.',
         '/'.

  LOOP AT GT_DD03L.
    CONCATENATE
      'SELECT-OPTIONS S_'
      GT_DD03L-FIELDNAME(6)
      ' FOR'
      INTO L_PARATAB.
    CONCATENATE
      L_PARATAB
      GT_DD03L-TABNAME
      INTO L_PARATAB SEPARATED BY SPACE.
    CONCATENATE
      L_PARATAB
      '-'
      GT_DD03L-FIELDNAME
      '.' INTO L_PARATAB.
    PERFORM MAKE_PRO USING : L_PARATAB, '/'.
    CLEAR L_PARATAB.
  ENDLOOP.

***  LOOP AT PARATAB WHERE LGGBN = 'P'.     "PARAMETER
***    PERFORM MAKE_PRO USING : PARATAB-LTEXT, '/'.
***  ENDLOOP.
  PERFORM MAKE_PRO USING :
         'SELECTION-SCREEN SKIP 1.', '/',
         'PARAMETERS: TESTRUN               AS CHECKBOX,', '/',
         '            CREATE    DEFAULT  ''X'' AS CHECKBOX,', '/',
         '            OBJECT    LIKE         ARCH_IDX-OBJECT', '/',
         '                      DEFAULT  ', OBJNAME , 'NO-DISPLAY',
         '.', '/',
         'SELECTION-SCREEN SKIP 1.', '/'.
  PERFORM MAKE_PRO USING :
        'PARAMETERS: COMMENT   LIKE ADMI_RUN-COMMENTS OBLIGATORY.','/',
        'SELECTION-SCREEN END OF BLOCK B2.', '/', '/',
        '***** Main login - common routine of include', '/',
        'PERFORM ARCHIVE_PROCESS.', '/', '/',
        '***** Common routine', '/',
        'INCLUDE ZITARCW.', '/', '/',
        '***** History for each object,', '/',
        '***** processing required for each part defined,', '/',
        'FORM OPEN_CURSOR_FOR_DB.', '/',
        '  OPEN CURSOR WITH HOLD G_CURSOR FOR', '/'.

  CONCATENATE
    'SELECT * FROM '
     P_TAB INTO L_PARATAB SEPARATED BY SPACE.
  PERFORM MAKE_PRO USING: L_PARATAB, '/'.

  DESCRIBE TABLE GT_DD03L LINES L_LIST_TOTALLINE.
  CONDENSE L_LIST_TOTALLINE.

  LOOP AT GT_DD03L.
    IF SY-TABIX EQ 1.
      CONCATENATE
        'WHERE'
        GT_DD03L-FIELDNAME INTO L_PARATAB SEPARATED BY SPACE.
      CONCATENATE
        L_PARATAB
        ' IN'
        ' S_'
        GT_DD03L-FIELDNAME(6)
      INTO L_PARATAB.
      PERFORM MAKE_PRO USING: L_PARATAB, '/'.
    ELSE.
      CONCATENATE
       'AND'
        GT_DD03L-FIELDNAME INTO L_PARATAB SEPARATED BY SPACE.
      CONCATENATE
        L_PARATAB
        ' IN'
        ' S_'
        GT_DD03L-FIELDNAME(6)
        INTO L_PARATAB.

      IF L_LIST_TOTALLINE EQ SY-TABIX.
        CONCATENATE
          L_PARATAB
          '.' INTO L_PARATAB.
      ENDIF.

      PERFORM MAKE_PRO USING: L_PARATAB, '/'.
    ENDIF.
  ENDLOOP.
***  LOOP AT PARATAB WHERE LGGBN = 'S'.     "SELECT
***    PERFORM  MAKE_PRO USING : PARATAB-LTEXT, '/'.
***  ENDLOOP.

  PERFORM MAKE_PRO USING :
*         SELECT1, '/',
*         SELECT2, '/',
*         SELECT3, '/',
*         SELECT4, '/',
*         SELECT5, '/',
*         SELECT6, '/',
         'ENDFORM.', '/',
         'FORM MAKE_ARCHIVE_OBJECT_ID.', '/',
         INDEX1, '/',
         INDEX2, '/',
         INDEX3, '/',
        'ENDFORM.', '/'.
ENDFORM.                    " make_archive_program
*---------------------------------------------------------------------*
*       FORM MAKE_delete_program                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MAKE_DELETE_PROGRAM.
  PERFORM MAKE_PRO USING :
'*&-------------------------------------------------------------*', '/',
'*& Report', P_DEL, '/',
'*&-------------------------------------------------------------*', '/',
'*System name         : HMI SYSTEM', '/',
'*Sub system name     : ARCHIVE', '/',
'*Program name        :', TEXTDEL, '/',
'*Program descrition  : Generated automatically by the ZHACR00800', '/',
'*Created on   :  ', SY-DATUM, '         Created by   :', SY-UNAME, '/',
'*Changed on :                           Changed by    :', '/',
'*Changed descrition :', '/',
'*"-------------------------------------------------------------*', '/',
         'REPORT',  P_DEL, '.', '/', '/',
         '***** Include TOP', '/',
         'INCLUDE', P_TOP, '.', '/', '/',
         '***** Selection screen.', '/',
         'PARAMETERS: TESTRUN               AS CHECKBOX,', '/',
         '            OBJECT    LIKE         ARCH_IDX-OBJECT', '/',
         '                      DEFAULT  ', OBJNAME, 'NO-DISPLAY',
           '.', '/', '/',
         '***** Main login - common routine of include', '/',
         'PERFORM DELETE_PROCESS.', '/', '/',
         '***** common routine', '/',
         'INCLUDE ZITARCD.', '/', '/',
         '***** History for each object,', '/',
         '***** processing required for each part defined,', '/',
         'FORM DELETE_FROM_TABLE.', '/',
         '  DELETE (ARC_TABLE) FROM TABLE T_ITAB.', '/',
         '  COMMIT WORK.', '/',
         '  CLEAR : T_ITAB, T_ITAB[].', '/',
         'ENDFORM.', '/'.
ENDFORM.                    "MAKE_DELETE_PROGRAM

*---------------------------------------------------------------------*
*       FORM MAKE_reload_PROGRAM                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MAKE_RELOAD_PROGRAM.
  PERFORM MAKE_PRO USING :
'*&-------------------------------------------------------------*', '/',
'*& Report', P_RLD, '/',
'*&-------------------------------------------------------------*', '/',
'*System name         : HMI SYSTEM', '/',
'*Sub system name     : ARCHIVE', '/',
'*Program name        :', TEXTRLD, '/',
'*Program descrition  : Generated automatically by the ZHACR00800', '/',
'*Created on   :  ', SY-DATUM, '         Created by   :', SY-UNAME, '/',
'*Changed on :                           Changed by    :', '/',
'*Changed descrition :', '/',
'*"-------------------------------------------------------------*', '/',
         'REPORT',  P_RLD, '.', '/', '/',
         '***** Include TOP ', '/',
         'INCLUDE', P_TOP, '.', '/', '/',
         '***** Selection screen.', '/',
         'PARAMETERS: TESTRUN               AS CHECKBOX,', '/',
         '            OBJECT    LIKE         ARCH_IDX-OBJECT', '/',
         '                      DEFAULT  ', OBJNAME, 'NO-DISPLAY',
         '.', '/', '/',
         '***** Main login - common routine of include', '/',
         'PERFORM RELOADING_PROCESS.', '/', '/',
         '***** Common routine', '/',
         'INCLUDE ZITARCL.', '/', '/',
         '***** History for each object,', '/',
         '***** processing required for each part defined,', '/',
         'FORM INSERT_FROM_TABLE.', '/',
         '  INSERT (ARC_TABLE) FROM TABLE T_ITAB.', '/',
         '  COMMIT WORK.', '/',
         '  CLEAR : T_ITAB, T_ITAB[].', '/',
         'ENDFORM.', '/'.
ENDFORM.                    "MAKE_RELOAD_PROGRAM

*---------------------------------------------------------------------*
*       FORM MAKE_read_PROGRAM                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MAKE_READ_PROGRAM.
  PERFORM MAKE_PRO USING :
'*&-------------------------------------------------------------*', '/',
'*& Report', P_REA, '/',
'*&-------------------------------------------------------------*', '/',
'*System name         : HMI SYSTEM', '/',
'*Sub system name     : ARCHIVE', '/',
'*Program name        :', TEXTREA, '/',
'*Program descrition  : Generated automatically by the ZHACR00800', '/',
'*Created on   :  ', SY-DATUM, '         Created by   :', SY-UNAME, '/',
'*Changed on :                           Changed by    :', '/',
'*Changed descrition :', '/',
'*"-------------------------------------------------------------*', '/',
         'REPORT',  P_REA, '.', '/', '/',
         '***** Include TOP ', '/',
         'INCLUDE', P_TOP, '.', '/', '/',
         '***** Selection screen.', '/',
         'PARAMETERS: OBJECT    LIKE         ARCH_IDX-OBJECT', '/',
         '                      DEFAULT  ', OBJNAME, 'NO-DISPLAY','.',
         '/', '/',
         '***** Main login - common routine of include', '/',
         'PERFORM READ_PROCESS.', '/', '/',
         '***** Common routine', '/',
         'INCLUDE ZITARCR.', '/', '/',
         '***** History for each object,', '/',
         '***** processing required for each part defined,', '/',
         'FORM WRITE_PROCESS.', '/'.

  LOOP AT GT_DD03L.

    IF SY-TABIX EQ 1.
      CONCATENATE
         'WRITE : / T_ITAB-'
         GT_DD03L-FIELDNAME
         ',' INTO L_PARATAB.
    ELSEIF SY-TABIX EQ L_LIST_TOTALLINE.
      CONCATENATE
       'T_ITAB-'
       GT_DD03L-FIELDNAME
       '.' INTO L_PARATAB.
    ELSE.
      CONCATENATE
         'T_ITAB-'
         GT_DD03L-FIELDNAME
         ',' INTO L_PARATAB.
    ENDIF.

    PERFORM MAKE_PRO USING: L_PARATAB, '/'.

  ENDLOOP.
  L_PARATAB = 'ENDFORM.'.
  PERFORM MAKE_PRO USING: L_PARATAB, '/'.

ENDFORM.                    "MAKE_READ_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  get_archive_parameters_fr_db
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ARCHIVE_PARAMETERS_FR_DB.
  SELECT * INTO TABLE PARATAB
    FROM ZITTACV002
    WHERE OBJECT     = P_OBJ.

ENDFORM.                    " get_archive_parameters_fr_db
*&---------------------------------------------------------------------*
*&      Form  MODIFY_TEXT_FOR_ARCHIVE_PROGRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_TEXT_FOR_ARCHIVE_PRO.
  DATA: TAB LIKE TEXTPOOL OCCURS 50 WITH HEADER LINE.

  READ TEXTPOOL P_PRO INTO TAB.
  READ TABLE TAB WITH KEY KEY = 'COMMENT'.
  IF SY-SUBRC <> 0.
    TAB-ID = 'S'. TAB-KEY = 'COMMENT'.
    TAB-ENTRY = '        Memo for archive execution'.
    COLLECT TAB. CLEAR TAB.
  ELSE.
    TAB-ENTRY = '        Memo for archive execution'.
    MODIFY  TAB INDEX SY-TABIX.
  ENDIF.
  READ TABLE TAB WITH KEY KEY = '001'.
  IF SY-SUBRC <> 0.
    TAB-ID = 'I'. TAB-KEY = '001'.  TAB-ENTRY = P_TEXT.
    COLLECT TAB. CLEAR TAB.
  ELSE.
    TAB-ENTRY = P_TEXT.
    MODIFY  TAB INDEX SY-TABIX.
  ENDIF.
  INSERT TEXTPOOL P_PRO FROM TAB LANGUAGE SY-LANGU.
  COMMIT WORK.
  TRDIRT-NAME  = P_PRO.
  TRDIRT-SPRSL = '3'.
  TRDIRT-TEXT  = TEXTACV.
  MODIFY TRDIRT.
  COMMIT WORK.

ENDFORM.                    " MODIFY_TEXT_FOR_ARCHIVE_PROGRA
*&---------------------------------------------------------------------*
*&      Form  set_attribute_program
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&2  text
*      -->P_&3  text
*      -->P_&4  text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTE_PROGRAM USING    P_REP
                                    P_TEXT
                                    P_ATT.
* #### ### Attribute ####
*****  UPDATE TRDIR SET     SUBC = P_ATT
*****                WHERE  NAME = P_REP.
*****  COMMIT WORK.


  GENERATE REPORT P_REP   MESSAGE G_ERROR.

  IF SY-SUBRC = 0.
    WRITE : / 'PROGRAM', P_REP ,'Succeed'.
    MESSAGE S000.
  ELSE.
    IF  P_ATT  = 'I'.
      WRITE : / 'PROGRAM', P_REP ,'Succeed'.
    ELSE.
      WRITE : / 'PROGRAM', P_REP ,'Fail', 'SY-SUBRC = ', SY-SUBRC,
                G_ERROR.
    ENDIF.
  ENDIF.

  TRDIRT-NAME  = P_REP.
  TRDIRT-SPRSL = '3'.
  TRDIRT-TEXT  = P_TEXT.
  MODIFY TRDIRT.

  COMMIT WORK.

ENDFORM.                    " set_attribute_program
*&---------------------------------------------------------------------*
*&      Form  CHECK_INVALID_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INVALID_OBJECT.
  SELECT *  FROM ARCH_DEF
    WHERE OBJECT = P_OBJ.
  ENDSELECT.
  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH '####' P_OBJ '## Config # ##'
                      '## #####'.
  ELSEIF P_OBJ(1) <> 'Z'.
    MESSAGE E000 WITH 'Standard ####' P_OBJ
                      '# #### ## #####'.
  ELSEIF SY-DBCNT > 1.
    MESSAGE E000 WITH '####' P_OBJ
                      '## #### 2# #####'
                      '#### ### # ####'.
  ENDIF.
ENDFORM.                    " CHECK_INVALID_OBJECT
