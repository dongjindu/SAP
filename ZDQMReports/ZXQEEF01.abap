*----------------------------------------------------------------------*
***INCLUDE ZXQEEF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_RESULT_DATE
*&---------------------------------------------------------------------*
FORM SET_RESULT_DATE USING    P_PRUEFLOS    "/Inspection Lot
                              P_VORGLFNR    "/Current Node Number
                              P_MERKNR      "/Insp. Char. Number
                              P_ART         "/Inspection type
                              P_MATNR       "/Material
                              P_WERKS       "/Plant
                              P_PRUEFDATUV  "/Start date of Inspection
                              P_PRUEFDATUB. "/End date of Inspection

  DATA : LW_FLD_NAME(40) TYPE C.
  FIELD-SYMBOLS : <LW_FS> TYPE D.
  DATA : LW_FLD_NAME_V(40) TYPE C.
  FIELD-SYMBOLS : <LW_FS_V> TYPE D.


  DATA : LW_ZTQM_QNS_ITEM   TYPE ZTQM_QNS_ITEM,
         LW_ZTQM_QNS_IT_MS  TYPE ZTQM_QNS_IT_MS.

  CASE P_ART.
    WHEN  C_INSP_TYPE_ISIR    OR
          C_INSP_TYPE_REGULAR .

      SELECT SINGLE *
         INTO CORRESPONDING FIELDS OF LW_ZTQM_QNS_ITEM
           FROM ZTQM_QNS_ITEM
             WHERE PRUEFLOS = P_PRUEFLOS
               AND I_STAT   = C_RELEASE.

    WHEN C_INSP_TYPE_MS.

      SELECT SINGLE *
         INTO CORRESPONDING FIELDS OF LW_ZTQM_QNS_IT_MS
           FROM ZTQM_QNS_IT_MS
             WHERE PRUEFLOS_MS = P_PRUEFLOS.

  ENDCASE.

  CHECK SY-SUBRC = 0.

*-- Get field location of MIC
  CASE P_ART.
    WHEN  C_INSP_TYPE_ISIR    OR
          C_INSP_TYPE_REGULAR .

      CONCATENATE 'LW_ZTQM_QNS_ITEM-DATUV_'
                   P_MERKNR
                     INTO  LW_FLD_NAME_V.

      ASSIGN (LW_FLD_NAME_V) TO <LW_FS_V>.

      CHECK NOT <LW_FS_V> IS INITIAL.

      CONCATENATE 'LW_ZTQM_QNS_ITEM-DATUB_'
                   P_MERKNR
                     INTO  LW_FLD_NAME.

      ASSIGN (LW_FLD_NAME) TO <LW_FS>.
      MOVE : P_PRUEFDATUB TO <LW_FS>.

      UPDATE ZTQM_QNS_ITEM  FROM  LW_ZTQM_QNS_ITEM.

    WHEN C_INSP_TYPE_MS.

      CONCATENATE 'LW_ZTQM_QNS_IT_MS-DATUV_1'
                   P_MERKNR+1(3)
                     INTO  LW_FLD_NAME_V.

      ASSIGN (LW_FLD_NAME_V) TO <LW_FS_V>.

      CHECK NOT <LW_FS_V> IS INITIAL.

      CONCATENATE 'LW_ZTQM_QNS_IT_MS-DATUB_1'
                   P_MERKNR+1(3)
                     INTO  LW_FLD_NAME.

      ASSIGN (LW_FLD_NAME) TO <LW_FS>.
      MOVE : P_PRUEFDATUB TO <LW_FS>.

      UPDATE ZTQM_QNS_IT_MS  FROM  LW_ZTQM_QNS_IT_MS.

  ENDCASE.

*  IF SY-SUBRC NE 0.
*    ROLLBACK WORK.
*    MESSAGE E000(ZMQM) WITH P_PRUEFLOS P_MATNR
*     'Can not transfer result date to scheduling'(E02).
*    EXIT.
*  ENDIF.
*
*  COMMIT WORK AND WAIT.


ENDFORM.                    " SET_RESULT_DATE
