*----------------------------------------------------------------------*
*   INCLUDE ZXQEEU06                                                   *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_QALS) LIKE  QALS STRUCTURE  QALS
*"             VALUE(I_QAPO) LIKE  QAPO STRUCTURE  QAPO
*"             VALUE(I_QAPP) LIKE  QAPP STRUCTURE  QAPP OPTIONAL
*"             VALUE(I_QALT) LIKE  QALT STRUCTURE  QALT OPTIONAL
*"             VALUE(I_QAMKR) LIKE  QAMKR STRUCTURE  QAMKR OPTIONAL
*"             VALUE(I_SINGLE_CHAR_KZ) LIKE  QM00-QKZ
*"       EXPORTING
*"             VALUE(E_QAMKR) LIKE  QAMKR STRUCTURE  QAMKR
*"       TABLES
*"              T_QAMKTAB STRUCTURE  QAMKR OPTIONAL

*<<<<<<<<<<<<<<<<<< ZQMEX_03 - Start >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
************************************************************************
* Enhancement Name  : ZQMEX_03
* Author            : SeungLyong, Lee
* Creation Date     : 2003.11.17.
* Specifications By : SeungLyong, Lee
* Pattern           : 4.5 Function Module Exit
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Transfer result date of Inspection lot
*                     to Inspection scheduling tables
*  Exit : QEEM0006 - add. functions after closing inspection characs
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
  DATA : LW_QAMKR TYPE QAMKR.

  CHECK I_QALS-ART = C_INSP_TYPE_ISIR    OR
        I_QALS-ART = C_INSP_TYPE_REGULAR OR
        I_QALS-ART = C_INSP_TYPE_MS.


  CHECK I_SINGLE_CHAR_KZ = 'X' OR
        NOT T_QAMKTAB IS INITIAL.

*-- Check single data
*-   Indicator(I_SINGLE_CHAR_KZ): X: I_QAMKR Filled, Otherwise T_QAMKTAB
  CASE 'X'.
    WHEN I_SINGLE_CHAR_KZ .
*-  Single Data
      CHECK NOT I_QAMKR-PRUEFDATUB IS INITIAL AND
                I_QAMKR-STATUSR = '5'.  "Completed
      PERFORM SET_RESULT_DATE  USING I_QALS-PRUEFLOS
                                     I_QAMKR-VORGLFNR
                                     I_QAMKR-MERKNR
                                     I_QALS-ART
                                     I_QALS-MATNR
                                     I_QALS-WERK
                                     I_QAMKR-PRUEFDATUV
                                     I_QAMKR-PRUEFDATUB.


    WHEN OTHERS.
*-  Characteristic Table (Only Filled if Indicator SPACE)
      LOOP AT T_QAMKTAB INTO LW_QAMKR.
        CHECK NOT LW_QAMKR-PRUEFDATUB IS INITIAL AND
                  LW_QAMKR-STATUSR = '5'.
        PERFORM SET_RESULT_DATE  USING LW_QAMKR-PRUEFLOS
                                       LW_QAMKR-VORGLFNR
                                       LW_QAMKR-MERKNR
                                       I_QALS-ART
                                       I_QALS-MATNR
                                       I_QALS-WERK
                                       LW_QAMKR-PRUEFDATUV
                                       LW_QAMKR-PRUEFDATUB.


      ENDLOOP.

  ENDCASE.

*<<<<<<<<<<<<<<<<<< ZQMEX_03 - End >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
