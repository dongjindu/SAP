*----------------------------------------------------------------------*
***INCLUDE ZLSVIMF04 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_NEW_ENTRIES
*&---------------------------------------------------------------------*
*       Creating LOG and Checking Field-Values (For Table ZTCO_VRRATIO)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_NEW_ENTRIES.
* This Rutine is triggered when creating a record in table ZTCO_VRRATIO.
* Creation Log
  ZTCO_VRRATIO-ERDAT = SY-DATUM.
  ZTCO_VRRATIO-ERZET = SY-UZEIT.
  ZTCO_VRRATIO-ERNAM = SY-UNAME.
* Clear Copied Log.
  CLEAR :
    ZTCO_VRRATIO-AEDAT ,
    ZTCO_VRRATIO-AEZET ,
    ZTCO_VRRATIO-AENAM .

ENDFORM. "CHECK_NEW_ENTRIES

*&---------------------------------------------------------------------*
*&      Form  CHECK_NEW_ENTRIES_02
*&---------------------------------------------------------------------*
*       Creating LOG and Checking Field-Values (For Table ZTCO_VRRATIO)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_NEW_ENTRIES_02.
* This Rutine is triggered
* when creating a record in table ZTCO_VRCCATCE
* Creation Log
  ZTCO_VRCCATCE-ERDAT = SY-DATUM.
  ZTCO_VRCCATCE-ERZET = SY-UZEIT.
  ZTCO_VRCCATCE-ERNAM = SY-UNAME.
* Clear Copied Log.
  CLEAR :
    ZTCO_VRCCATCE-AEDAT ,
    ZTCO_VRCCATCE-AEZET ,
    ZTCO_VRCCATCE-AENAM .

ENDFORM. "CHECK_NEW_ENTRIES_02

*&---------------------------------------------------------------------*
*&      Form  CHK_EXPIRED_DATE
*&---------------------------------------------------------------------*
*        Check Cost Center Expiration Date
*----------------------------------------------------------------------*
*      -->P_KOSTL       CCtr
*      -->P_NAME_COALL  CCtr Grp
*----------------------------------------------------------------------*
FORM CHK_EXPIRED_DATE.

  DATA : LV_KOKRS LIKE TKA01-KOKRS. "Controlling Area
  DATA : LV_BUKRS LIKE TKA02-BUKRS. "Company Code
  DATA : BEGIN OF IT_L_BUKRS OCCURS 0,
           BUKRS LIKE T001-BUKRS,
         END OF IT_L_BUKRS.
  DATA : IT_L_FIELDS   LIKE	HELP_VALUE OCCURS 0
                       WITH HEADER  LINE.

* get Controlling Area.
  GET PARAMETER ID 'CAC' FIELD LV_KOKRS.
  IF LV_KOKRS EQ SPACE.
    CALL TRANSACTION 'OKKS'.
    GET PARAMETER ID 'CAC' FIELD LV_KOKRS.
  ENDIF.

* Get CCode List (Company Code List)
  CALL FUNCTION 'RK_BUKRS_OF_KOKRS'
       EXPORTING
            KOKRS          = LV_KOKRS
       TABLES
            T_BUKRS        = IT_L_BUKRS
       EXCEPTIONS
            NO_BUKRS_FOUND = 1
            NOT_FOUND      = 2
            OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Company Code Select - using a Modal Dialogue
  DESCRIBE TABLE IT_L_BUKRS LINES SY-TFILL.
  IF SY-TFILL = 1.
    CLEAR  IT_L_BUKRS.
    READ TABLE IT_L_BUKRS INDEX 1.
    LV_BUKRS = IT_L_BUKRS-BUKRS.
  ELSE.
    IT_L_FIELDS-TABNAME   = 'T001'.
    IT_L_FIELDS-FIELDNAME = 'BUKRS'.
    APPEND IT_L_FIELDS.
    CALL FUNCTION 'SLWY_POP_SELECT_VALUE_FROM_TAB'
         EXPORTING
              TITEL        = 'Company Code Selection'
         IMPORTING
              SELECT_VALUE = LV_BUKRS
         TABLES
              FIELDS       = IT_L_FIELDS
              VALUETAB     = IT_L_BUKRS
         EXCEPTIONS
              INVALID_DATA = 1
              OTHERS       = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

* Check Costcenter Existance and Expiration Date
  CALL FUNCTION 'ITOB_CHECK_COSTCENTER'
       EXPORTING
            KOKRS_IMP         = LV_KOKRS
            KOSTL_IMP         = ZTCO_VRRATIO-KOSTL
            BUKRS_IMP         = LV_BUKRS
       EXCEPTIONS
            EMPTY_KEY         = 1
            APPLICATION_ERROR = 2
            OTHERS            = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CHK_EXPIRED_DATE

*&---------------------------------------------------------------------*
*&      Form  CHK_REL_CC_N_CCG_FROM_HARCH
*&---------------------------------------------------------------------*
*       Read/Check CCG CCtr relationship from Hierarchy
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_REL_CC_N_CCG_FROM_HARCH.

** Def. Required Fields
  DATA : LV_KOKRS    LIKE TKA01-KOKRS. "Controlling Area
  DATA : IT_L_NODES  TYPE GSETH_NODE_TAB
                          WITH HEADER LINE ,
         IT_L_VALUES TYPE GSETH_VAL_TAB
                          WITH HEADER LINE .
* get Controlling Area.
  GET PARAMETER ID 'CAC' FIELD LV_KOKRS.
  IF LV_KOKRS EQ SPACE.
    CALL TRANSACTION 'OKKS'.
    GET PARAMETER ID 'CAC' FIELD LV_KOKRS.
  ENDIF.

* Read Hierarchy From Object ID
  PERFORM READ_HI_FR_SETID  TABLES IT_L_NODES
                                   IT_L_VALUES
                            USING  LV_KOKRS
                                   '0101'
                                   ZTCO_VRRATIO-NAME_COALL.
* Find if CC user input is included to the selected CCgr...
  LOOP AT IT_L_VALUES WHERE VFROM <= ZTCO_VRRATIO-KOSTL
                        AND VTO   >= ZTCO_VRRATIO-KOSTL.
  ENDLOOP.
  IF SY-SUBRC = 0.
  ELSE.
    MESSAGE E002(ZMCO) WITH ZTCO_VRRATIO-KOSTL
                            ZTCO_VRRATIO-NAME_COALL.
  ENDIF.

ENDFORM.                    " CHK_REL_CC_N_CCG_FROM_HARCH

*&---------------------------------------------------------------------*
*&      Form  CHANGE_LOG
*&---------------------------------------------------------------------*
*       Change Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_LOG.
  IF <ACTION> = 'U'.
    ZTCO_VRRATIO-AEDAT = SY-DATUM.
    ZTCO_VRRATIO-AEZET = SY-UZEIT.
    ZTCO_VRRATIO-AENAM = SY-UNAME.
  ENDIF.
ENDFORM.                    " CHANGE_LOG

*&---------------------------------------------------------------------*
*&      Form  read_hi_fr_Setid
*&---------------------------------------------------------------------*
*       Read Hierarchy From OBJ ID
*----------------------------------------------------------------------*
*      -->IT_L_NODES  For Node Data
*      -->IT_L_VALUES For Value Data
*      -->LV_KOKRS    Controlling Area
*      -->P_Class     Class
*      -->P_GRP_NAME  Group Name
*----------------------------------------------------------------------*
FORM READ_HI_FR_SETID  TABLES IT_L_NODES  STRUCTURE GRPOBJECTS
                              IT_L_VALUES STRUCTURE GRPVALUES
                       USING  LV_KOKRS    LIKE      TKA01-KOKRS
                              P_CLASS
                              P_GRP_NAME.
* Def. Local Data
  DATA : VL_E_SETID     LIKE SETHIER-SETID,
         VL_C_OVERWRITE LIKE SY-DATAR     ,
         VL_C_INFO      LIKE GRPHINFO     .

  CLEAR : IT_L_NODES , IT_L_NODES[],
          IT_L_VALUES, IT_L_VALUES[].
  CLEAR : VL_E_SETID, VL_C_OVERWRITE, VL_C_INFO .

* Def. SET ID
  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      SETCLASS                   = P_CLASS
      SHORTNAME                  = P_GRP_NAME
      KOKRS                      = LV_KOKRS
*     KTOPL                      =
*     LIB                        =
*     RNAME                      =
*     ECCS_DIMEN                 =
*     ECCS_ITCLG                 =
*     ECCS_SITYP                 =
   IMPORTING
      SETID                      = VL_E_SETID
   EXCEPTIONS
      NO_CO_AREA_SPECIFIED       = 1
      ILLEGAL_SETCLASS           = 2
      OTHERS                     = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CONDENSE VL_E_SETID NO-GAPS.

* Look Up hierarchy of Costelement Group .
  CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
       EXPORTING
            E_CLASS                     = P_CLASS
            E_SETID                     = VL_E_SETID
            E_KOKRS                     = LV_KOKRS
            E_MANDT                     = SY-MANDT
       TABLES
            T_NODES                     = IT_L_NODES
            T_VALUES                    = IT_L_VALUES
       CHANGING
            C_INFO                      = VL_C_INFO
            C_OVERWRITE                 = VL_C_OVERWRITE
       EXCEPTIONS
            NO_CONTROLLING_AREA         = 1
            NO_CHART_OF_ACCOUNT         = 2
            DIFFERENT_CONTROLLING_AREAS = 3
            DIFFERENT_CHART_OF_ACCOUNTS = 4
            SET_NOT_FOUND               = 5
            ILLEGAL_FIELD_REPLACEMENT   = 6
            ILLEGAL_TABLE_REPLACEMENT   = 7
            FM_RAISE                    = 8
            CONVERT_ERROR               = 9
            NO_OVERWRITE_STANDARD_HIER  = 10
            NO_BUKRS_FOR_KOKRS          = 11
            OTHERS                      = 12.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " read_hi_fr_Setid

*&---------------------------------------------------------------------*
*&      Form  CHK_CCGRP_INCLUDING
*&---------------------------------------------------------------------*
*       Check CCgrp
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_CCGRP_INCLUDING.

** Def. Required Fields
  DATA : LV_KOKRS    LIKE TKA01-KOKRS. "Controlling Area
  DATA : IT_L_NODES  TYPE GSETH_NODE_TAB
                          WITH HEADER LINE ,
         IT_L_VALUES TYPE GSETH_VAL_TAB
                          WITH HEADER LINE .
* get Controlling Area.
  GET PARAMETER ID 'CAC' FIELD LV_KOKRS.
  IF LV_KOKRS EQ SPACE.
    CALL TRANSACTION 'OKKS'.
    GET PARAMETER ID 'CAC' FIELD LV_KOKRS.
  ENDIF.

* Read Hierarchy From Object ID
  PERFORM READ_HI_FR_SETID  TABLES IT_L_NODES
                                   IT_L_VALUES
                            USING  LV_KOKRS
                                   '0101'
                                   GV_CCGR_SETID.
* Check Including
  LOOP AT IT_L_NODES  WHERE HLEVEL <> '0'
                        AND SHORTNAME = ZTCO_VRRATIO-NAME_COALL.
  ENDLOOP.
  IF SY-SUBRC = 0.
  ELSE.
    MESSAGE E000(ZMCO) WITH ZTCO_VRRATIO-NAME_COALL
                            ' is not under the Basic Grp '
                            GV_CCGR_SETID.
  ENDIF.
ENDFORM.                    " CHK_CCGRP_INCLUDING
