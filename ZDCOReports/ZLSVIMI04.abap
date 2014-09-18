*----------------------------------------------------------------------*
*   INCLUDE ZLSVIMI04                                                  *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  CHK_RELATION_CC_N_CCG  INPUT
*&---------------------------------------------------------------------*
*       Check Input Fields (Relationship with CC and CCG )
*       On Screen 0001
*----------------------------------------------------------------------*
MODULE CHK_RELATION_CC_N_CCG INPUT.
*Cost Center       : ZTCO_VRRATIO-KOSTL
*Cost Center Group : ZTCO_VRRATIO-NAME_COALL

* Check Cost Center Expiration Date
  IF NOT ZTCO_VRRATIO-KOSTL IS INITIAL.
    PERFORM CHK_EXPIRED_DATE .
  ENDIF.

* Check CCgrp
  IF ZTCO_VRRATIO-NAME_COALL NE SPACE.
    PERFORM CHK_CCGRP_INCLUDING.
  ENDIF.

* CCGrp-CC relationship?
  IF     NOT ZTCO_VRRATIO-KOSTL IS INITIAL
     AND     ZTCO_VRRATIO-NAME_COALL NE SPACE.
    PERFORM CHK_REL_CC_N_CCG_FROM_HARCH.
  ENDIF.

*  BREAK-POINT.
ENDMODULE.                 " CHK_RELATION_CC_N_CCG  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHK_INIT_VAL  INPUT
*&---------------------------------------------------------------------*
*       Check_Init_Values
*----------------------------------------------------------------------*
MODULE CHK_INIT_VAL INPUT.

  IF  ZTCO_VRRATIO-GJAHR       IS INITIAL
*   OR ZTCO_VRRATIO-PERBL       IS INITIAL
   OR ZTCO_VRRATIO-NAME_COALL  IS INITIAL
*   OR ZTCO_VRRATIO-KOSTL       IS INITIAL
   OR ZTCO_VRRATIO-KSTAR       IS INITIAL.
    MESSAGE E001(ZMCO) WITH 'Input'.
  ENDIF.

ENDMODULE.                 " CHK_INIT_VAL  INPUT

*&---------------------------------------------------------------------*
*&      Module  INPUT_CCG_GROUP  INPUT
*&---------------------------------------------------------------------*
*       SH for CCgroup
*----------------------------------------------------------------------*
MODULE INPUT_CCG_GROUP INPUT.
  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS                  = 'X'
      CLASS                    = '0101'
*     CRUSER                   = '*'
      FIELD_NAME               = SPACE
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      SEARCHFLD_REQUIRED       = ' '
*     SET                      = GV_CCGR_SETID
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    IMPORTING
*     CLASS_NAME               =
      SET_NAME                 = ZTCO_VRRATIO-NAME_COALL
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    EXCEPTIONS
      NO_SET_PICKED            = 1
      OTHERS                   = 2.

* No error Processing At F4 Help
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDMODULE.                 " INPUT_CCG_GROUP  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHANGE_LOG_W_VRRATIO  INPUT
*&---------------------------------------------------------------------*
*       Leave Change log
*----------------------------------------------------------------------*
MODULE CHANGE_LOG_W_VRRATIO INPUT.
  PERFORM CHANGE_LOG.
ENDMODULE.                 " CHANGE_LOG_W_VRRATIO  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHK_INIT_DATA  INPUT
*&---------------------------------------------------------------------*
*       Check Init Value (Table ZTCO_VRCCATCE )
*----------------------------------------------------------------------*
MODULE CHK_INIT_DATA INPUT.
* Check Initial Value
  IF    ZTCO_VRCCATCE-KOKRS IS INITIAL
     OR ZTCO_VRCCATCE-KOSTL IS INITIAL
     OR ZTCO_VRCCATCE-KSTAR IS INITIAL
     OR ZTCO_VRCCATCE-LSTAR IS INITIAL.
    MESSAGE E001(ZMCO) WITH 'Input'.
  ENDIF.

* Check Relationship between CCtr and Activity Type
  CALL FUNCTION 'K_COSTCENTER_ACTIVITY_READ'
    EXPORTING
*     GET_OBJNR               = ' '
      GJAHR                   = SY-DATUM(4)
      KOKRS                   = ZTCO_VRCCATCE-KOKRS
      KOSTL                   = ZTCO_VRCCATCE-KOSTL
      LSTAR                   = ZTCO_VRCCATCE-LSTAR
*     GET_NONALLOC            = ' '
*   IMPORTING
*     AUSEH                   =
*     AUSFK                   =
*     LATYP                   =
*     LEINH                   =
*     OBJNR                   =
*     LATYPI                  =
    EXCEPTIONS
      NOT_FOUND               = 1
      NOT_FOUND_IN_YEAR       = 2
      NOT_NORMAL              = 3
      OTHERS                  = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDMODULE.                 " CHK_INIT_DATA  INPUT

*&---------------------------------------------------------------------*
*&      Module  INPUT_ACT_TYPE  INPUT
*&---------------------------------------------------------------------*
*       F4 for Validactivity type as CCtr
*----------------------------------------------------------------------*
MODULE INPUT_ACT_TYPE INPUT.
*
*  CALL FUNCTION 'C_VALID_COSTCENTER_ACTIVITIES'
*    EXPORTING
*      DATE                = SY-DATUM
*      KOKRS               = ZTCO_VRCCATCE-KOKRS
*      KOSTL               = ZTCO_VRCCATCE-KOSTL
**     KOSTL_FLDNAME       = ' '
**     USE_BUFFER          = 'X'
**     WINX1               = '0'
**     WINY1               = '0'
*    IMPORTING
*      LSTAR               = ZTCO_VRCCATCE-LSTAR .

ENDMODULE.                 " INPUT_ACT_TYPE  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_LOG_ZTCO_VEHI_TYPE  INPUT
*&---------------------------------------------------------------------*
*       Log For ZTCO_VEHI_TYPE
*----------------------------------------------------------------------*
MODULE SET_LOG_ZTCO_VEHI_TYPE INPUT.
  IF     STATUS-ACTION = 'U'.
    ZTCO_VEHI_TYPE-AEDAT = SY-DATUM.
    ZTCO_VEHI_TYPE-AEZET = SY-UZEIT.
    ZTCO_VEHI_TYPE-AENAM = SY-UNAME.
  ELSEIF STATUS-ACTION = 'A'.
    ZTCO_VEHI_TYPE-ERDAT = SY-DATUM.
    ZTCO_VEHI_TYPE-ERZET = SY-UZEIT.
    ZTCO_VEHI_TYPE-ERNAM = SY-UNAME.
  ENDIF.
ENDMODULE.                 " SET_LOG_ZTCO_VEHI_TYPE  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHK_MATNR_CHG  INPUT
*&---------------------------------------------------------------------*
*       Check Init Value and Material Type  (Table ZTCO_BUSPLANBOM )
*----------------------------------------------------------------------*
MODULE CHK_MATNR_CHG INPUT.
** Check Initial Value
*  IF ZTCO_BUSPLANBOM-MATNR_CHG IS INITIAL.
*    MESSAGE E001(ZMCO) WITH 'Input' ZTCO_BUSPLANBOM-MATNR_CHG.
*  ENDIF.

* Check Material Type
  DATA : LV_MTART_ORG LIKE MARA-MTART.
  DATA : LV_MTART_CHG LIKE MARA-MTART.

  CLEAR : LV_MTART_ORG, LV_MTART_CHG .

  SELECT SINGLE MTART INTO LV_MTART_ORG
                      FROM MARA
                     WHERE MATNR =  ZTCO_BUSPLANBOM-MATNR.

  SELECT SINGLE MTART INTO LV_MTART_CHG
                      FROM MARA
                     WHERE MATNR =  ZTCO_BUSPLANBOM-MATNR_CHG.

  IF LV_MTART_ORG <> LV_MTART_CHG .
    MESSAGE W027(ZMCO) WITH ZTCO_BUSPLANBOM-MATNR_CHG
                            ZTCO_BUSPLANBOM-MATNR.
  ENDIF.

* Check The relationship with Plant
  CHECK NOT ZTCO_BUSPLANBOM-MATNR_CHG IS INITIAL.

  DATA : LV_WERKS LIKE MARC-WERKS.
  CLEAR  LV_WERKS.
  SELECT SINGLE WERKS
                  INTO LV_WERKS
                  FROM MARC
                 WHERE MATNR =  ZTCO_BUSPLANBOM-MATNR_CHG
                   AND WERKS =  ZTCO_BUSPLANBOM-WERKS .

  IF SY-SUBRC <> 0.
    MESSAGE E028(ZMCO) WITH ZTCO_BUSPLANBOM-MATNR_CHG.
  ENDIF.

ENDMODULE.                 " CHK_MATNR_CHG  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_LOG_ZTCO_MHAATYPE  INPUT
*&---------------------------------------------------------------------*
*       Log for ZTCO_MHAATYPE
*----------------------------------------------------------------------*
MODULE SET_LOG_ZTCO_MHAATYPE INPUT.
  IF     STATUS-ACTION = 'U'.
    ZTCO_MHAATYPE-AEDAT = SY-DATUM.
    ZTCO_MHAATYPE-AEZET = SY-UZEIT.
    ZTCO_MHAATYPE-AENAM = SY-UNAME.
  ELSEIF STATUS-ACTION = 'A'.
    ZTCO_MHAATYPE-ERDAT = SY-DATUM.
    ZTCO_MHAATYPE-ERZET = SY-UZEIT.
    ZTCO_MHAATYPE-ERNAM = SY-UNAME.
  ENDIF.
ENDMODULE.                 " SET_LOG_ZTCO_MHAATYPE  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHK_VAL_RANGE  INPUT
*&---------------------------------------------------------------------*
*       Check Value Range
*----------------------------------------------------------------------*
MODULE CHK_VAL_RANGE INPUT.
  RANGES : R_L_AWART FOR ZTCO_MHAATYPE-AWFROM.
  CLEAR  : R_L_AWART, R_L_AWART[].
  DATA   : WA_L_ZTCO_MHAATYPE LIKE ZTCO_MHAATYPE.

* Check Range
  IF    ZTCO_MHAATYPE-AWFROM NE SPACE
    AND ZTCO_MHAATYPE-AWTO   NE SPACE.
    IF ZTCO_MHAATYPE-AWFROM > ZTCO_MHAATYPE-AWTO.
      MESSAGE E034.
    ENDIF.
    R_L_AWART-LOW  = ZTCO_MHAATYPE-AWFROM.
    R_L_AWART-HIGH = ZTCO_MHAATYPE-AWTO.
    R_L_AWART-SIGN = 'I'.
    R_L_AWART-OPTION = 'BT'.
    APPEND   R_L_AWART.
  ENDIF.

  IF    ZTCO_MHAATYPE-AWFROM EQ SPACE
    AND ZTCO_MHAATYPE-AWTO   NE SPACE.
    ZTCO_MHAATYPE-AWFROM = ZTCO_MHAATYPE-AWTO.
    CLEAR ZTCO_MHAATYPE-AWTO.
    R_L_AWART-LOW  = ZTCO_MHAATYPE-AWFROM.
*   R_L_AWART-high = ZTCO_MHAATYPE-AWTO.
    R_L_AWART-SIGN = 'I'.
    R_L_AWART-OPTION = 'EQ'.
    APPEND   R_L_AWART.
  ENDIF.

  IF    ZTCO_MHAATYPE-AWFROM NE SPACE
    AND ZTCO_MHAATYPE-AWTO   EQ SPACE.
    R_L_AWART-LOW  = ZTCO_MHAATYPE-AWFROM.
*   R_L_AWART-high = ZTCO_MHAATYPE-AWTO.
    R_L_AWART-SIGN = 'I'.
    R_L_AWART-OPTION = 'EQ'.
    APPEND   R_L_AWART.
  ENDIF.
* Check Duplication
*  LOOP AT TOTAL INTO WA_L_ZTCO_MHAATYPE.
*    IF WA_L_ZTCO_MHAATYPE-PRDIND <> ZTCO_MHAATYPE-PRDIND.
*      IF WA_L_ZTCO_MHAATYPE-AWFROM IN R_L_AWART
*      OR WA_L_ZTCO_MHAATYPE-AWTO   IN R_L_AWART.
*        MESSAGE E041(ZMCO).
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

ENDMODULE.                 " CHK_VAL_RANGE  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_LOG_ZTCO_EBUSPLANBOM  INPUT
*&---------------------------------------------------------------------*
*       Log for ZTCO_EBUSPLANBOM
*----------------------------------------------------------------------*
MODULE SET_LOG_ZTCO_EBUSPLANBOM INPUT.
  IF     STATUS-ACTION = 'U'.
*    ZTCO_EBUSPLANBOM-AEDAT = SY-DATUM.
*    ZTCO_EBUSPLANBOM-AEZET = SY-UZEIT.
*    ZTCO_EBUSPLANBOM-AENAM = SY-UNAME.
  ENDIF.
ENDMODULE.                 " SET_LOG_ZTCO_EBUSPLANBOM  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_LOG_ZTCO_WARR  INPUT
*&---------------------------------------------------------------------*
*       Log For ZTCO_WARR
*----------------------------------------------------------------------*
MODULE SET_LOG_ZTCO_WARR INPUT.
  IF     STATUS-ACTION = 'U'.
    ZTCO_WARR-AEDAT = SY-DATUM.
    ZTCO_WARR-AEZET = SY-UZEIT.
    ZTCO_WARR-AENAM = SY-UNAME.
  ELSEIF STATUS-ACTION = 'A'.
    ZTCO_WARR-ERDAT = SY-DATUM.
    ZTCO_WARR-ERZET = SY-UZEIT.
    ZTCO_WARR-ERNAM = SY-UNAME.
  ENDIF.
ENDMODULE.                 " SET_LOG_ZTCO_WARR  INPUT
