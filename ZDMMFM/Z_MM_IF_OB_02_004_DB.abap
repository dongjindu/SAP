FUNCTION Z_MM_IF_OB_02_004_DB.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_DATE) LIKE  SY-DATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"----------------------------------------------------------------------

  CLEAR : IT_M047, IT_M047[].

  G_DEST = 'WMPM01'.

  SELECT B~MATNR
         B~WERKS
         B~LGORT
         B~LABST
         B~SPEME
         INTO CORRESPONDING FIELDS OF TABLE IT_M047
         FROM MARA AS A
        INNER JOIN MARD AS B
           ON A~MATNR EQ B~MATNR
        INNER JOIN T001L AS C
           ON B~WERKS EQ C~WERKS
          AND B~LGORT EQ C~LGORT
* Engine Plant Split ( by IG Moon 12/13/2011 )
* {     WHERE B~WERKS IN ('E001', 'P001')
        WHERE B~WERKS IN ('E001', 'P001','E002')
* }
          AND A~MTART = 'ROH'
      AND A~LVORM EQ SPACE
      AND B~LVORM EQ SPACE
*S__Change By Paul 04/29/11
*          and B~lgort IN ('P400','E100', 'E200', 'G100')

* Engine Plant Split ( by IG Moon 12/13/2011 )
*          AND B~LGORT IN ('P400','E100', 'E200', 'G100', 'G150')
          AND B~LGORT IN ('P400','E100', 'E200', 'G100', 'G150','N100', 'N200')
* }
      AND A~PROFL IN ('V','K','M')
*	    AND A~TEMPB NE '11'
** Furong on 09/09/11
*           AND MSTAE IN ('11', '12', '13')
** Furong on 02/15/13
*    	    AND A~TEMPB IN ('3', '4', '9', ' ')
          AND A~TEMPB IN ('2', '3', '4', '9', ' ')
** End on 02/15/13
*         AND A~TEMPB NOT IN ('1', '2')
** End on 09/09/11
          AND C~DISKZ EQ SPACE.

  IT_M047-ZCRTDT    = SY-DATUM.
  IT_M047-ZCRTIM    = SY-UZEIT.
  MODIFY IT_M047 TRANSPORTING ZCRTDT ZCRTIM
                        WHERE TYPE = ' '.


  CHECK NOT IT_M047[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_02_004'
    DESTINATION G_DEST
    IMPORTING
      E_RETURN              = E_RETURN
    TABLES
      IT_BODY               = IT_M047
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1
      SYSTEM_FAILURE        = 2
      RESOURCE_FAILURE      = 3
      OTHERS                = 4.

  CASE SY-SUBRC.
    WHEN '1'.
      E_RETURN-MESSAGE = 'communication_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '2'.
      E_RETURN-MESSAGE = 'system_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '3'.
      E_RETURN-MESSAGE = 'resource_failure'.
      E_RETURN-TYPE    = 'E'.
    WHEN '4'.
      E_RETURN-MESSAGE = 'Others'.
      E_RETURN-TYPE    = 'E'.
  ENDCASE.

  IT_M047-TYPE     = E_RETURN-TYPE.
  IT_M047-ETDAT    = SY-DATUM.
  IT_M047-ETTIM    = SY-UZEIT.
  IT_M047-ETNAM    = SY-UNAME.
  IT_M047-MESSAGE  = E_RETURN-MESSAGE.
  MODIFY IT_M047 TRANSPORTING TYPE ETDAT ETTIM ETNAM MESSAGE
                        WHERE TYPE = ' '.

  MODIFY ZMMT0047 FROM TABLE IT_M047.

  COMMIT WORK.

*-- Error Log -----------------------------------
*  data : it_m048 like it_m047 occurs 0 with header line.
*
*  loop at it_m047 to 1000.
*    move-corresponding it_m047 to it_m048.
*    append it_m048.
*  endloop.

** Furong on 08/18/2011
  IF SY-TCODE = 'ZMMR00016'.
  ELSE.
** on 08/18/2011

   CALL METHOD ZMMC_CL_IF=>IF_SET_KEY(  IFKEY = 'MMIF204_ECC_OB'
                               MODLE = 'GCS'       " 'MM', 'SD', 'FI'
                               CENTY = 'US'       " '
                               DIRCT = 'O'  " 'O':Outbound, 'I':Inbound
                               LOGOX = ' '
                               TTYPE = 'S'
                               CPARM = '4'
                             ).
    LOOP AT IT_M047.

      GV_RETURN =  E_RETURN-TYPE.



      CALL METHOD ZMMC_CL_IF=>IF_SET_MESSG( TYPE    = E_RETURN-TYPE
                                ID      = ' '    "gt_retmsg-id
                                MESSAGE = E_RETURN-MESSAGE
                              ).

      CALL METHOD ZMMC_CL_IF=>IF_SET_PARAM( ISTAT = GV_RETURN

                                IFP01 = IT_M047-ZCRTDT
                                IFP02 = IT_M047-ZCRTIM
                                IFP03 = IT_M047-MATNR
                                IFP04 = IT_M047-LGORT
                              ).

    ENDLOOP.

    CALL METHOD ZMMC_CL_IF=>IF_SAVE_DATA( ).
  ENDIF.
ENDFUNCTION.
