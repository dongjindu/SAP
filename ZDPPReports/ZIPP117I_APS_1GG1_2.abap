REPORT ZIPP117I_APS_1GG1_2 NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMPP.
************************************************************************
* Program Name      : ZIPP117I_APS_1GG1_2
* Author            :
* Creation Date     : 10/07/08
* Specifications By :
* Addl Documentation: APS II - Daily Input Result
* Description       :
*
************************************************************************
* Modification Logs
* Date        Developer    RequestNo    Description
*
************************************************************************

TABLES: ZTPP_PSS01GG.

DATA: IT_PSS01GG LIKE TABLE OF ZTPP_PSS01GG WITH HEADER LINE.



CONSTANTS: C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS: P_DATE LIKE SY-DATUM.
*SELECT-OPTIONS: S_DATE FOR ZTPP_pss01gg-BDAT.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_EAI          TYPE C AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) TEXT-100 FOR FIELD P_EAI.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B1.

INITIALIZATION.
  PERFORM  INITILIZE.

START-OF-SELECTION.
  PERFORM DATA_SELECT.
  IF IT_PSS01GG[] IS INITIAL.
    MESSAGE I001 WITH TEXT-001.
  ELSE.
    IF P_EAI = 'X'.
      PERFORM SEND_DATA.
    ENDIF.
    PERFORM DATA_UPDATE.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.
  DATA: L_SUBRC    TYPE SY-SUBRC ,
        L_NAME     TYPE CABN-ATNAM,
        L_ATFLV    TYPE AUSP-ATFLV,
        L_NUM(08)  TYPE N,
        L_CDATE LIKE SY-DATUM,
        L_CTIME LIKE SY-UZEIT.

  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK    LIKE AUSP-OBJEK,
          ATWRT    LIKE AUSP-ATWRT,
        END OF LT_OBJEK.

  L_CDATE = SY-DATUM.
  L_CTIME = SY-UZEIT.

  L_NAME = 'P_RP01_SHOP_DATE'.
  L_ATFLV = L_NUM = P_DATE.

  SELECT DISTINCT OBJEK
    INTO TABLE LT_OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE KLART = '002' AND
          AU~ATFLV = L_ATFLV AND
          CA~ATNAM = L_NAME .

  CLEAR: IT_PSS01GG, IT_PSS01GG[].

  LOOP AT LT_OBJEK.
    CLEAR IT_PSS01GG.

*    MOVE-CORRESPONDING lt_objek TO it_pss01gg.

    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK
                                             'P_MODEL'
                                       CHANGING IT_PSS01GG-MODL .

*    PERFORM read_normal_class_app244 USING LT_OBJEK-objek
*                                             'P_BODY_SERIAL'
*                                       CHANGING IT_PSS01GG-bodyno .
*    CONCATENATE IT_PSS01GG-MODL IT_PSS01GG-bodyno
*      INTO IT_PSS01GG-bodyno .

    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK
                                             'P_DESTINATION_CODE'
                                       CHANGING IT_PSS01GG-DIST.

    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK
                                             'P_MI'
                                       CHANGING IT_PSS01GG-BMDL.

    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK
                                             'P_OCN'
                                       CHANGING IT_PSS01GG-OCNN.

    PERFORM READ_NORMAL_CLASS USING LT_OBJEK-OBJEK
                                             'P_VERSION'
                                       CHANGING IT_PSS01GG-VERS.

*    CONCATENATE 'P_RP' l_rpno '_SERIAL'
*      INTO l_name.
*    PERFORM read_normal_class USING    LT_OBJEK-objek
*                                                l_name
*                                       CHANGING LT_OBJEK-serial .

    PERFORM READ_NORMAL_CLASS USING    LT_OBJEK-OBJEK
                                                'P_WORK_ORDER'
                                       CHANGING IT_PSS01GG-ORDR.

    PERFORM READ_NORMAL_CLASS USING    LT_OBJEK-OBJEK
                                                'P_EXT_COLOR'
                                        CHANGING IT_PSS01GG-EXTC.

*    PERFORM read_normal_class USING    LT_OBJEK-objek
*                                                'P_PLAN_ORDER'
*                                        CHANGING it_pss01gg-vendor.


    PERFORM READ_NORMAL_CLASS USING    LT_OBJEK-OBJEK
                                                'P_INT_COLOR'
                                       CHANGING IT_PSS01GG-INTC.

*    CONCATENATE 'P_RP' l_rpno '_ACTUAL_DATE'
*      INTO l_name .
*    PERFORM read_normal_class_app244 USING    LT_OBJEK-objek
*                                                l_name
*                                       CHANGING l_atwrt .
*    IT_PSS01GG-act_date = l_atwrt+00(08).
*    IT_PSS01GG-act_time = l_atwrt+08(06).

    IT_PSS01GG-BDAT = P_DATE.
    IT_PSS01GG-PLNT = '1'.
    IT_PSS01GG-BQTY = 1.
    IT_PSS01GG-STTM = L_CDATE.
    IT_PSS01GG-ERZET = L_CTIME.
    COLLECT IT_PSS01GG.
  ENDLOOP.
ENDFORM.                    " DATA_SELECT

*---------------------------------------------------------------------*
*       FORM read_normal_class                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VMNO                                                        *
*  -->  P_CHAR                                                        *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM READ_NORMAL_CLASS USING    P_VMNO  P_CHAR
                                CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .
ENDFORM.                    " read_normal_classification

*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
FORM DATA_UPDATE.
  DATA: L_TEXT(60) TYPE C,
        L_INT TYPE I.

  DELETE FROM ZTPP_PSS01GG CLIENT SPECIFIED WHERE MANDT = SY-MANDT.

  INSERT ZTPP_PSS01GG FROM TABLE IT_PSS01GG.

  IF SY-SUBRC = 0.
    DESCRIBE TABLE IT_PSS01GG LINES L_INT.
    WRITE L_INT TO L_TEXT LEFT-JUSTIFIED.
    COMMIT WORK.
    CONCATENATE 'Created Record count :' L_TEXT
      INTO L_TEXT.
    MESSAGE  S001 WITH L_TEXT.
    MESSAGE  S001 WITH TEXT-002.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  W001 WITH TEXT-003.
  ENDIF.
ENDFORM.                    " DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_DATA.
  DATA : L_MSGTXT(100),
         L_RESULT(1).

  CALL FUNCTION 'Z_FPP_SET_PSS01GG_2'
    DESTINATION C_DEST
    IMPORTING
      FLAG          = L_RESULT
    TABLES
      I_PSS01GG     = IT_PSS01GG
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

  IF L_RESULT = 'S'.
    MESSAGE I001 WITH 'Successfully sent out'.
  ELSE.
    IF L_RESULT IS INITIAL.
      L_RESULT = 'E'.
    ENDIF.
    MESSAGE I001 WITH L_MSGTXT.
  ENDIF.
  LOOP AT IT_PSS01GG.
    IT_PSS01GG-INT_FLAG = L_RESULT.
    MODIFY IT_PSS01GG TRANSPORTING INT_FLAG.
  ENDLOOP.
ENDFORM.                    " SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  INITILIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITILIZE.
  P_DATE = SY-DATUM - 1.
ENDFORM.                    " INITILIZE
