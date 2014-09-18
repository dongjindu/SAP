*&-------------------------------------------------------------------
*& Report  ZOBJECT_TABLE
*& T-code:DB15
*& Find out the relation between tables and archiving objects.
*& [Procedure]
*& First, Upload the table for analysis in txt format.
*&-------------------------------------------------------------------


REPORT  ZOBJECT_TABLE LINE-SIZE 140 .
TABLES: ARCH_DEF, DD02L, DD02T.
TYPES: BEGIN OF LINE,
         SON    LIKE DD02L-TABNAME,
       END OF LINE.
DATA: LIN TYPE LINE,
      TAB TYPE TABLE OF LINE WITH HEADER LINE.
DATA: BEGIN OF TAB1 OCCURS 0 ,
      OBJECT LIKE ARCH_DEF-OBJECT,
      SON    LIKE ARCH_DEF-SON,
      END OF TAB1.
DATA: BEGIN OF TEXT,
        TEXT LIKE DD02T-DDTEXT,
      END OF TEXT.
TYPES:
  BEGIN OF ADK_CCMS_TABLE,
    TABNAME LIKE DD02V-TABNAME,
    TEXT    LIKE DD02V-DDTEXT,
  END OF ADK_CCMS_TABLE,

  ADK_CCMS_TABLES TYPE ADK_CCMS_TABLE OCCURS 0,

  BEGIN OF ADK_CCMS_OBJECT,
    OBJECT LIKE ARCH_DEF-OBJECT,
    OBJTEXT LIKE ARCH_TXT-OBJTEXT,
    SIGN(1) TYPE C,
    TABLES TYPE ADK_CCMS_TABLES,
  END OF ADK_CCMS_OBJECT,

  ADK_CCMS_OBJECTS TYPE ADK_CCMS_OBJECT OCCURS 0.
DATA : ARCHOBJ TYPE ADK_CCMS_OBJECTS.
DATA:BEGIN OF ITAB OCCURS 0,
       OBJECT LIKE ARCH_DEF-OBJECT,
       OBJTEXT LIKE ARCH_TXT-OBJTEXT,
       SIGN(1) TYPE C,
       TABLES TYPE ADK_CCMS_TABLES,
    END OF ITAB.
DATA:BEGIN OF ITAB1 OCCURS 0,
       TABLENAME LIKE DD02L-TABNAME,
       TABLETEXT LIKE DD02T-DDTEXT,
       OBJECT LIKE ARCH_DEF-OBJECT,
       OBJTEXT LIKE ARCH_TXT-OBJTEXT,
    END OF ITAB1.


*###### ###### ###### #### ###### #### ###### ######## ##
CALL FUNCTION 'UPLOAD'
     EXPORTING
          CODEPAGE            = ' '
          FILENAME            = 'c:\table.txt'
          FILETYPE            = ' '
          ITEM                = 'Read Test f? Excel File'(002)
*     IMPORTING
*          FILESIZE            = ' '
*          ACT_FILENAME        = ' '
*          ACT_FILETYPE        = ' '
     TABLES
          DATA_TAB            = TAB
     EXCEPTIONS
          CONVERSION_ERROR    = 1
          INVALID_TABLE_WIDTH = 2
          INVALID_TYPE        = 3.
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
*########## #### ######## #### ########## ######## ###### ##
LOOP AT TAB.
  CALL FUNCTION 'ADK_CCMS_GET_OBJECTS'
    EXPORTING
      TABLE                            = TAB-SON
*   DELETE_OBJECTS_ONLY              = 'X'
*   GET_ALL_TABLES                   = ' '
    TABLES
      OBJECTS                          = ARCHOBJ
   EXCEPTIONS
     TABLE_NOT_FOUND                  = 1
     POOL_OR_CLUSTER_NOT_IN_USE       = 2
     NO_OBJECT_FOUND                  = 3
     TABLE_IS_NOT_TRANSPARENT         = 4
     OTHERS                           = 5.
*######## #### ######## ########## ####
  IF SY-SUBRC = 3.
    SELECT SINGLE DDTEXT FROM DD02T INTO TEXT
              WHERE TABNAME = TAB-SON
                AND DDLANGUAGE = 'E'.
    ITAB1-TABLETEXT = TEXT.
    ITAB1-TABLENAME = TAB-SON.
    ITAB1-OBJECT = ' '.
    ITAB1-OBJTEXT = ' '.
    APPEND ITAB1.

    CONTINUE.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  INSERT LINES OF ARCHOBJ INTO TABLE ITAB.
  LOOP AT ITAB.

    SELECT SINGLE DDTEXT FROM DD02T INTO TEXT
            WHERE TABNAME = TAB-SON
              AND DDLANGUAGE = 'E'.
    ITAB1-TABLETEXT = TEXT.
    ITAB1-TABLENAME = TAB-SON.
    ITAB1-OBJECT = ITAB-OBJECT.
    ITAB1-OBJTEXT = ITAB-OBJTEXT.
    APPEND ITAB1.
  ENDLOOP.
  REFRESH ITAB.
  CLEAR ITAB.
  REFRESH ARCHOBJ.
  CLEAR TAB-SON.
ENDLOOP.
SORT ITAB1 BY TABLENAME.
LOOP AT ITAB1.
  WRITE:/1(10) ITAB1-TABLENAME,
         11    ITAB1-TABLETEXT,
         75(10) ITAB1-OBJECT,
         93   ITAB1-OBJTEXT .

ENDLOOP.
