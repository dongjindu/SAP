************************************************************************
* Program Name      : ZACO53U_MM_BK
* Author            : Eun Hwa , Jung
* Creation Date     : 2004.03.31
* Specifications By : Bong-Doo , Moon
* Pattern           : Report 1-1
* Development Request No : UD
* Addl Documentation:
* Description       : material master block release

* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT zaco53u_mm_bk MESSAGE-ID zmco.

TABLES: MARA.
DATA: BEGIN OF it_mara OCCURS 0,
       matnr TYPE mara-matnr,
       lvorm TYPE mara-lvorm,
       mtart TYPE mara-mtart,
       mbrsh TYPE mara-mbrsh,
       matkl TYPE mara-matkl,
       mstae TYPE mara-mstae,
     END OF it_mara.

DATA: BEGIN OF it_mbew OCCURS 0,
       matnr TYPE mbew-matnr,
       bwkey TYPE mbew-bwkey,
       zplp1 TYPE mbew-zplp1,
       lplpr TYPE mbew-lplpr,
     END OF it_mbew.

* BAPI
DATA : w_headdata LIKE bapimathead,
       w_clientdata LIKE bapi_mara,
       w_clientdatax LIKE bapi_marax,
       w_plantdata   LIKE bapi_marc,
       w_plantdatax   LIKE  bapi_marcx.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                        WITH HEADER LINE.
DATA : BEGIN OF it_return2  OCCURS 0,
       message(100),
      END OF it_return2.
data: w_err ,
      g_message(100).

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: P_MATNR  FOR MARA-MATNR .
PARAMETERS :
             mark(1) DEFAULT 'X'.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK bl1.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Read Information from MARA+MBEW
  PERFORM read_mara_mbew.
* Call bapi function
  PERFORM call_bapi.

*&---------------------------------------------------------------------*
*&      Form  READ_MARA_MBEW
*&---------------------------------------------------------------------*
*       Read Information from MARA+MBEW
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mara_mbew.
  data: l_line type i.

  IF mark = 'X'.
  ELSE.
    MESSAGE e000(zmco) WITH ' input value error '.
  ENDIF.


  CLEAR : it_mara, it_mara[].
  CLEAR : it_mbew, it_mbew[].

  SELECT matnr mtart mbrsh mstae
         INTO CORRESPONDING FIELDS OF TABLE it_mara
         FROM mara
         WHERE mtart IN  ('ROH1', 'ROH')
*Issue Number : CO-20041202-001, Requested by  JC Jeong
*Changed on 2004/12/02, by WSKIM
*---Start
*          AND MSTAE NE '12'.
         AND ( mstae EQ space OR mstae EQ '11' )
         AND MATNR IN P_MATNR.
*---End
  CLEAR it_mara.
  DESCRIBE TABLE IT_MARA LINES L_LINE.

  IF L_LINE = 0.
    W_ERR = 'X'.
    G_MESSAGE  = 'No material selected'.
    exit.
  ENDIF.


  SORT it_mara BY matnr.


  SELECT matnr bwkey zplp1 lplpr
         INTO CORRESPONDING FIELDS OF TABLE it_mbew
         FROM mbew
         FOR ALL ENTRIES IN it_mara
         WHERE matnr = it_mara-matnr
*// Mod. By Hyung Jin Youn  2004.06.03
* Check only Standard Price (Current) (Not Initial)
*               AND ( ZPLP1 > 0 OR LPLPR > 0 ).
               AND stprs NE space.
*// End of Mod.
  CLEAR it_mbew.
  SORT it_mbew BY matnr.




ENDFORM.                    " READ_MARA_MBEW
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi.

  CLEAR : it_return, it_return[].
  CLEAR : it_return2, it_return2[].
  CLEAR : w_headdata,
          w_clientdata,  w_clientdatax,
          w_plantdata,  w_plantdatax .

  IF it_mbew[]  IS INITIAL .
    WRITE : ' Material master blocking data not found '.
    STOP.
  ENDIF.

  CLEAR it_mbew.
  LOOP AT it_mbew.
*    if sy-tabix = 5.    " TEST
*      exit.
*    else.
    CLEAR : w_headdata,
          w_clientdata,  w_clientdatax,
          w_plantdata,  w_plantdatax .
    CLEAR it_mara.
    READ TABLE it_mara WITH KEY matnr = it_mbew-matnr.

    w_headdata-material = it_mbew-matnr.
    w_headdata-ind_sector = it_mara-mbrsh.
    w_headdata-matl_type = it_mara-mtart.
    w_headdata-basic_view = 'X'.
    w_headdata-purchase_view = 'X'.
*    w_headdata-MATERIAL_EXTERNAL = '1011706161'.

    w_clientdata-pur_status = '12'.
    w_clientdatax-pur_status = 'X'.
    w_plantdata-plant = it_mbew-bwkey.
    w_plantdata-pur_status = '12'.
    w_plantdatax-plant = it_mbew-bwkey.
    w_plantdatax-pur_status = 'X'.


*  BAPI_MATERIAL_SAVEDATA
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'               "  blocking
      EXPORTING
        headdata                   = w_headdata
        clientdata                 = w_clientdata
        clientdatax                = w_clientdatax
        plantdata                  = w_plantdata
        plantdatax                 = w_plantdatax
     IMPORTING
        return                     = it_return   .


    IF it_return-type = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
    CONCATENATE it_mbew-matnr ':' it_return-message INTO it_return2.
    APPEND it_return2.
    CLEAR  it_return2.
*    endif.
  ENDLOOP.

  IF sy-subrc = 0.
    LOOP AT it_return2.
      WRITE /: it_return2-message.
    ENDLOOP.
  ENDIF.



ENDFORM.                    " CALL_BAPI
