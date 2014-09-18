************************************************************************
* Program Name      : ZIQM01U_VEHICLE_INFO_V1
* Author            : SeungLyong, Lee
* Creation Date     : 2004.02.16.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.1
* Development Request No : UD1K901760
* Addl Documentation: UD1K907440
* Description       : Vehicle Information interface with GQIS
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT  ZIQM01U_VEHICLE_INFO_V1    NO STANDARD PAGE HEADING  .

*&&& Data Declaration.  &&&*
*TYPE-POOLS VRM.     "//Value Request Manager: Types & Constants
*TYPE-POOLS CXTAB .  "//Table_control Object type pool
*TABLES : FELD.      "//Screen Object Structure

*-- Include Program ( Include Constants or etc)
INCLUDE <ICON>.

*-- SAP Scripts Object Interface
*TABLES : THEAD. "/SAPscript: Text Header

*//Tables;(TABLES : Table_Name /View "//Table Description)
TABLES : AUSP, "/Characteristic Values
         CABN. "/Characteristic

*//Structures Declaration(TABLES : Structure Name."/Description)
TABLES : ZSQM_IQS_VEH.       "/Interface str. for KD_VIN_MAST(GQIS)
TABLES : ZSCA_CHARACTERISTIC_VALUE, "/Condition for Read
         ZSCA_CHAR_VALUE,           "/Required Characteristic Value
         ZSCA_VEHICLE_CHAR_VALUE.   "/Characteristic Values


*//InfoType;()
*//Cluster or Import Parameter;(Parameter Name)

*//Controls(for only Screen Control Element);(TC_ , or TS_)

*//Type (Table Structure);(TY_ )- Table or Structure

*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'
CONSTANTS : C_MARK   VALUE 'X'.

**//-- Global : used Variable just in this Program
*-- Function Control
DATA : OK_CODE LIKE SY-UCOMM.
*--
DATA : WA_RETURN     LIKE	BAPIRETURN1.   "Return Values

*-- User Confirm for pop-up Message
DATA : WA_ANSWER TYPE C.
DATA : WA_REPID LIKE SY-REPID.

*//-- Work area Variables in Program.(WA_xxxx)
*-   Period for vehicle master data
DATA : WA_ATWRT_L TYPE ATWRT, "/Start date
       WA_ATWRT_H TYPE ATWRT. "/End date

*DATA : WA_ATFLV_L TYPE ATFLV, "/Start date
*       WA_ATFLV_H TYPE ATFLV. "/End date

DATA : WA_DATE_TEMP TYPE DATUM.

DATA : WA_ATNAM	LIKE  CABN-ATNAM VALUE 'P_RP18_SHOP_DATE',
       WA_COUNT	TYPE  I  VALUE 1000000.


*-- Data Level control

*//Data(Work Area or (Internal) Structures);(WA_ )(ST_)?

*//Internal Tables and Index Fields;(IT_), (I_)
*-    Value of characteristic for Vehicle master
*- Condition for Read
DATA : IT_CONDITION   LIKE  ZSCA_CHARACTERISTIC_VALUE OCCURS 0
                                                  WITH HEADER LINE.
*- Required Characteristic Value
DATA : IT_VALUE       LIKE  ZSCA_CHAR_VALUE  OCCURS 0
                                                  WITH HEADER LINE.
*- Characteristic Values
DATA : IT_VEHICLE     LIKE  ZSCA_VEHICLE_CHAR_VALUE OCCURS 0	
                                                  WITH HEADER LINE.

*- Interface str. for KD_VIN_MAST(GQIS) :Vin information table
DATA : IT_ZSQM_IQS_VEH LIKE ZSQM_IQS_VEH OCCURS 0 WITH HEADER LINE.


*-- Characteristic master
DATA : BEGIN OF IT_CABN  OCCURS 10,
         ATINN  TYPE ATINN,    "/Characteristic Number
         ATNAM  TYPE ATNAM,    "/Characteristic Name
         ATFOR  TYPE ATFOR,    "/Value data type
       END OF IT_CABN.

*//Ranges; (R_)
*-- range of Required Characteristic
RANGES : R_ATNAM  FOR CABN-ATNAM.

*//Field Symbols; <FS_>
*-- TABLE CONTROLS VARIABLE(field-symbols)

*//Field Group;

* Control Framework Basic Class


***//Macro Definitions
*-- macro : macro_name &1 &2
*--           &1 -
*--           &2 -
DEFINE ATINN_CHAR.
  CLEAR &1.
  MOVE : &2  TO &1-ATNAM.
  APPEND &1.
END-OF-DEFINITION.


***//& Selection Screen Definition(Parameters Select-Option)
*-- Paramerters : (P_), Select-Options : (S_)
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
PARAMETERS : P_DATE  TYPE DATUM  DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK BLK .

*-- Seclection Screen Flow Logic Control Event Handling
*AT SELECTION-SCREEN ON ( ON END OF, ON VALUE-REQUEST FOR,
* ON HELP-REQUEST FOR, ON RADIOBUTTON GROUP, ON BLOCK OUTPUT,
* ON EXIT-COMMAND )
AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR '1000'.

AT SELECTION-SCREEN ON BLOCK BLK .
  CHECK SY-UCOMM = 'ONLI'.

  WA_DATE_TEMP  =  P_DATE - 10.

  WA_ATWRT_L = WA_DATE_TEMP.
  WA_ATWRT_H = P_DATE.



START-OF-SELECTION.
*-- Select Vehicle master using Vehicle master Function
  PERFORM GET_VEHICLE_MASTER. "/Get vehicle Master Info


**-- End of Selection.
END-OF-SELECTION.

  CHECK NOT IT_VEHICLE[] IS INITIAL.
*-  Mapping data for interface to IT_ZSQM_IQS_VEH
  PERFORM MAP_CHAR_VALUE_2_GQIS.




*// Event Handling(Except Selection Screen (Flow)event)
LOAD-OF-PROGRAM.
  REFRESH IT_VALUE.

  ATINN_CHAR : IT_VALUE   'P_VIN',            "/Vehicle
               IT_VALUE   'P_RP18_SHOP_DATE', "/Shopdate
               IT_VALUE   'P_ENGINE_NO',      "/Engine Number
               IT_VALUE   'P_219_9',          "/Engine capacity
               IT_VALUE   'P_219_8',          "/Fuel Type
               IT_VALUE   'P_TM_NO',          "/TM Number
               IT_VALUE   'P_219_7',          "/TM Type
               IT_VALUE   'P_MODEL',       "/Vehicle type code
               IT_VALUE   'P_WORK_ORDER',     "/Work order
               IT_VALUE   'P_KEY_NO'.         "/Key Number

*-- Get characteristic data from CABN using Characteristic Value name
  REFRESH IT_CABN.

  SELECT ATINN ATNAM ATFOR
      INTO CORRESPONDING FIELDS OF TABLE IT_CABN
        FROM CABN
          WHERE ATNAM  IN R_ATNAM.

  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH
         'There is no characteristic for GQIS Interface'(E01).
    EXIT.
  ENDIF.


INITIALIZATION.


**<<<<<<<<< Program Main / Subroutine / Flow Logic >>>>>>>>>>>>**
*&-----------------------------------------------------------------*
*&      Form  GET_VEHICLE_MASTER
*&-----------------------------------------------------------------*
FORM GET_VEHICLE_MASTER.

  DATA : LW_HIT_COUNT TYPE I.

  CALL FUNCTION 'Z_FCA_GET_VEHICLE_MASTER'
       EXPORTING
            I_ATNAM                       = WA_ATNAM
            I_ATWRT_S                     = WA_ATWRT_L
            I_ATWRT_E                     = WA_ATWRT_H
*            I_OBJEK                       =
            I_COUNT                       = WA_COUNT
       IMPORTING
            E_HIT_COUNT                   = LW_HIT_COUNT
       TABLES
            T_CONDITION                   = IT_CONDITION
            T_VALUE                       = IT_VALUE
            T_VEHICLE                     = IT_VEHICLE
       EXCEPTIONS
            DATE_OVERFLOW                 = 1
            INVALID_DATE                  = 2
            CONDITION_DOES_NOT_EXIST      = 3
            CHARACTERISTIC_DOES_NOT_EXIST = 4
            OTHERS                        = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " GET_VEHICLE_MASTER
*&------------------------------------------------------------------*
*&      Form  MAP_CHAR_VALUE_2_GQIS
*&------------------------------------------------------------------*
FORM MAP_CHAR_VALUE_2_GQIS.

  DATA : LW_INDEX LIKE SY-TABIX.

  REFRESH IT_ZSQM_IQS_VEH.

  SORT IT_VEHICLE BY OBJEK ASCENDING. " Sort

  LOOP AT IT_VEHICLE.

*    CLEAR IT_ZSQM_IQS_VEH.
*    READ TABLE IT_CABN WITH KEY ATNAM = IT_VEHICLE-ATNAM.
*    CHECK SY-SUBRC = 0.

    CASE IT_VEHICLE-ATNAM.
      WHEN 'P_VIN'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-VIN_NO.
      WHEN 'P_RP18_SHOP_DATE'.
        MOVE : IT_VEHICLE-ATWRT+22(8) TO IT_ZSQM_IQS_VEH-PROD_DATE.
      WHEN 'P_ENGINE_NO'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-ENGN_NO.
      WHEN 'P_219_9'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-ENGN_CAPA.
      WHEN 'P_219_8'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-FUEL_TYP.
      WHEN 'P_TM_NO'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-TM_NO.
      WHEN 'P_219_7'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-TM_TYP.
      WHEN 'P_MODEL'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-VIN_MODEL.
      WHEN 'P_WORK_ORDER'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-WK_ORD.
      WHEN 'P_KEY_NO'.
        MOVE : IT_VEHICLE-ATWRT TO IT_ZSQM_IQS_VEH-KEY_NO.

      WHEN OTHERS.

    ENDCASE.

    AT END OF OBJEK.
      APPEND IT_ZSQM_IQS_VEH.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " MAP_CHAR_VALUE_2_GQIS
