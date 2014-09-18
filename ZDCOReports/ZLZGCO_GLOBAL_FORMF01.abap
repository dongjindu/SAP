*----------------------------------------------------------------------*
*   INCLUDE ZLZGCO_GLOBAL_FORMF01                                      *
*----------------------------------------------------------------------*

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
