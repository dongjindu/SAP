************************************************************************
* Program Name      : ZAPP902R_ADJUST_VEHICLE
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'ADJUST_VEHICLE_MASTER
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP902R_ADJUST_VEHICLE    NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ausp.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: it_ausp                LIKE TABLE OF ausp        WITH HEADER LINE,
      IT_VALS                LIKE TABLE OF ZSPP_VIN_VALUE
                                                       WITH HEADER LINE,
      BEGIN OF it_aehd OCCURS 0,
        sqdt(8), "TYPE ZTPP_PMT07JB_A-SQDT,
      END OF it_aehd.

*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
DATA: wa_error                 TYPE c,
      wa_check                 TYPE c,
      wa_atinn                 LIKE ausp-atinn,
      wa_atnam                 LIKE cabn-atnam.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS:
  p_char                     LIKE cabn-atnam  OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b1.

START-OF-SELECTION.
  PERFORM check_characteristic .
  PERFORM update_process       .

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  CHECK_CHARACTERISTIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_characteristic.
  DATA: l_clint              LIKE kssk-clint.

    select single clint into l_clint
      from KLAH
     where CLASS = 'P_VEHICLE_MASTER'.

    SELECT objek INTO CORRESPONDING FIELDS OF TABLE IT_AUSP
      FROM KSSK
     WHERE klart = '002'
       AND clint = l_clint  .

   DELETE IT_AUSP WHERE OBJEK < 'EMF002001'  OR OBJEK > 'EMF003400' .
ENDFORM.                    " CHECK_CHARACTERISTIC

*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_process.
  DATA: L_AUSP               LIKE AUSP    ,
        L_VALS               LIKE TABLE OF IT_VALS     WITH HEADER LINE,
        L_WOSUM              LIKE ZTPP_WOSUM,
        L_MTART              LIKE MARA-MTART,
        L_MATNR              LIKE MARA-MATNR,
        l_atinn              LIKE wa_atinn.

  CHECK wa_error = space .
  L_VALS-ATNAM = 'P_WORK_ORDER'.        APPEND L_VALS .
  L_VALS-ATNAM = 'P_EXT_COLOR' .        APPEND L_VALS .
  L_VALS-ATNAM = 'P_INT_COLOR' .        APPEND L_VALS .
  CASE p_char        .
    WHEN 'P_FLEET'   .
      CLEAR: IT_VALS, IT_VALS[].
      IT_VALS-ATNAM = 'P_FLEET'   .     APPEND IT_VALS.
      LOOP AT it_ausp.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
          tables
            val_table          = L_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CLEAR: L_MATNR.
        LOOP AT L_VALS.
          CONCATENATE L_MATNR L_VALS-ATWRT INTO L_MATNR.
        ENDLOOP.

        CHECK L_MATNR NE SPACE .
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = L_MATNR
            CTYPE              = '001'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
            MODE               = 'W'
            CTYPE              = '002'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
      ENDLOOP.
    WHEN 'P_REGION_PORT'   .
      CLEAR: IT_VALS, IT_VALS[].
      IT_VALS-ATNAM = 'P_REGION_PORT'.  APPEND IT_VALS.
      LOOP AT it_ausp.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
          tables
            val_table          = L_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CLEAR: L_MATNR.
        LOOP AT L_VALS.
          CONCATENATE L_MATNR L_VALS-ATWRT INTO L_MATNR.
        ENDLOOP.

        CHECK L_MATNR NE SPACE .
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = L_MATNR
            CTYPE              = '001'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
            MODE               = 'W'
            CTYPE              = '002'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
      ENDLOOP.
    WHEN 'P_COLOR_SER'     .
      CLEAR: IT_VALS, IT_VALS[].
      IT_VALS-ATNAM = 'P_COLOR_SER'  .  APPEND IT_VALS.
      LOOP AT it_ausp.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
          tables
            val_table          = L_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CLEAR: L_MATNR.
        LOOP AT L_VALS.
          CONCATENATE L_MATNR L_VALS-ATWRT INTO L_MATNR.
        ENDLOOP.

        CHECK L_MATNR NE SPACE .
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = L_MATNR
            CTYPE              = '001'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
            MODE               = 'W'
            CTYPE              = '002'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
      ENDLOOP.
    WHEN 'P_RP_STATUS'     .
      CLEAR: IT_VALS, IT_VALS[].
      IT_VALS-ATNAM = 'P_RP_STATUS'  .
      IT_VALS-ATWRT = '00'           .  APPEND IT_VALS.
      LOOP AT it_ausp.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
            MODE               = 'W'
            CTYPE              = '002'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
      ENDLOOP.
    WHEN 'P_STATUS'        .
      CLEAR: IT_VALS, IT_VALS[].
      IT_VALS-ATNAM = 'P_STATUS'     .
      IT_VALS-ATWRT = 'B00'          .  APPEND IT_VALS.
      LOOP AT it_ausp.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
            MODE               = 'W'
            CTYPE              = '002'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
      ENDLOOP.
    WHEN 'P_MANUAL_ORDER'  .
      CLEAR: IT_VALS, IT_VALS[].
      IT_VALS-ATNAM = 'P_MANUAL_ORDER'. APPEND IT_VALS.
      LOOP AT it_ausp.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
          tables
            val_table          = L_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CLEAR: L_MATNR.
        LOOP AT L_VALS.
          CONCATENATE L_MATNR L_VALS-ATWRT INTO L_MATNR.
        ENDLOOP.

        CHECK L_MATNR NE SPACE .
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = L_MATNR
            CTYPE              = '001'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
            MODE               = 'W'
            CTYPE              = '002'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
      ENDLOOP.
    WHEN 'P_ORDER_ZONE'    .
      CLEAR: IT_VALS, IT_VALS[].
      IT_VALS-ATNAM = 'P_ORDER_ZONE'  . APPEND IT_VALS.
      LOOP AT it_ausp.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
          tables
            val_table          = L_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CLEAR: L_MATNR.
        LOOP AT L_VALS.
          CONCATENATE L_MATNR L_VALS-ATWRT INTO L_MATNR.
        ENDLOOP.

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = L_MATNR
            CTYPE              = '001'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object             = IT_AUSP-OBJEK(18)
            MODE               = 'W'
            CTYPE              = '002'
          tables
            val_table          = IT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " UPDATE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0111   text
*      -->P_L_ATINN  text
*----------------------------------------------------------------------*
FORM get_atinn USING    pa_char   pa_atinn.
  SELECT SINGLE atinn INTO pa_atinn
    FROM cabn
   WHERE atnam = pa_char .
ENDFORM.                    " GET_ATINN
