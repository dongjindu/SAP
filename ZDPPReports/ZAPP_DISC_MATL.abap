************************************************************************
* Program Name      : ZAPP_DISC_MATL
* Creation Date     : 09/2009
* Development Request No :
* Addl Documentation:
* Description       : Material Discontinuation in Planned Orders
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZAPP_DISC_MATL NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_DATUM LIKE SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM INIT_DATA.

START-OF-SELECTION.

  PERFORM PROCESS_DATA.


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: LT_DCM_LIST LIKE TABLE OF ZTPP_DCM_LIST WITH HEADER LINE.
  DATA: L_DATE LIKE SY-DATUM.

 SELECT A~MATNR AS FRMATNR NFMAT AS TOMATNR AUSDT LABST LABST AS BALQTY
    INTO CORRESPONDING FIELDS OF TABLE  LT_DCM_LIST
    FROM MARC AS A
    INNER JOIN MARD AS B
    ON A~WERKS = B~WERKS
    AND A~MATNR = B~MATNR
    WHERE B~LGORT = 'P400'
      AND A~KZAUS <> ' '
      AND A~AUSDT <= P_DATUM
      AND B~LABST > 0.

  IF SY-SUBRC = 0.
    LOOP AT LT_DCM_LIST.
      SELECT SINGLE SORTF MAX( DATUV )  INTO (LT_DCM_LIST-SORTF,L_DATE)
        FROM STPO
        WHERE IDNRK = LT_DCM_LIST-FRMATNR
          AND DATUV =< P_DATUM
          GROUP BY SORTF.
      IF SY-SUBRC = 0.
        MODIFY LT_DCM_LIST.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF LT_DCM_LIST[] IS INITIAL.
  ELSE.
    DELETE FROM ZTPP_DCM_LIST WHERE FRMATNR <> ' ' OR TOMATNR = ' '.
    MODIFY ZTPP_DCM_LIST FROM TABLE LT_DCM_LIST.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  P_DATUM = SY-DATUM - 1.
ENDFORM.                    " init_data
