************************************************************************
* Program Name      : ZAPP903R_ALC_SUM
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       : APP903: VEHICLE SCHEDULING
*
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
************************************************************************
REPORT  ZAPP903R_ALC_SUM      MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ztpp_common_vals,
        ztpp_input_plan ,
        ZTPP_ALC_SUM    .

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF it_data        OCCURS 0,
        objek                 LIKE ausp-objek.      " Vehicle Code
        INCLUDE STRUCTURE     ztpp_input_plan.
DATA: END OF it_data.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: wa_data             LIKE it_data                             ,
      WA_PRJ              TYPE C,
      WA_WBS              TYPE C,
      wa_wdate            LIKE sy-datum                            ,
      wa_uzeit            LIKE sy-uzeit                            ,
      wa_index            LIKE sy-tabix                            ,
      wa_hour             type i                                   ,
      wa_serial           LIKE ztpp_dvrt1-serial                   .

*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* INCLUDE PROGRAM DECLATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
* PERFORM setting_default.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM read_inputplan .
  PERFORM create_REAL    .

*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------



*&---------------------------------------------------------------------*
*&      Form  CREATE_REAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_REAL   .
  DELETE IT_DATA  WHERE STATUS = SPACE .
  MODIFY ZTPP_ALC_SUM  FROM TABLE IT_DATA.
ENDFORM.                    " CREATE_REAL

*&---------------------------------------------------------------------*
*&      Form  READ_INPUTPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_inputplan.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_input_plan .

  DELETE FROM ZTPP_ALC_SUM    CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  setting_default
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_default.
** Considering : If 2 Model is running, The UPH is input the 2 Values..
**               If More Model is running, How to programming????
*  DATA: l_jobs                TYPE programm  ,
*        l_lrate               LIKE ldlh-lrate.
*
*  GET TIME.    CLEAR: wa_prj, wa_wbs.
*  p_model = 'EMF'   .
*  SELECT SINGLE lrate INTO l_lrate
*    FROM crhd AS c INNER JOIN ldlh AS l
*      ON c~objid = l~lnid
*   WHERE c~arbpl = '1'  .
*  IF sy-subrc NE 0.
*    p_uph = 63   .
*  ELSE.
*    p_uph = l_lrate .
*  ENDIF.
*
*  CLEAR: ztpp_common_vals.  l_jobs = sy-repid.
*  SELECT SINGLE *
*    FROM ztpp_common_vals
*   WHERE jobs = l_jobs
*     AND key2 = 'WBS' .
*
*  IF sy-subrc = 0.
*    p_wbs    = ztpp_common_vals-item4 .
*  ELSE.
*    wa_wbs   = 'X'.
*    p_wbs    = 4 .
*  ENDIF.
*
*  CLEAR: ztpp_common_vals.  l_jobs = sy-repid.
*  SELECT SINGLE *
*    FROM ztpp_common_vals
*   WHERE jobs = l_jobs
*     AND key2 = 'PRJ' .
*
*  IF sy-subrc = 0.
*    p_prj    = ztpp_common_vals-item4 .
*  ELSE.
*    wa_prj   = 'X'.
*    p_prj    = 20.
*  ENDIF.
ENDFORM.                    " SETTING_DEFAULT
