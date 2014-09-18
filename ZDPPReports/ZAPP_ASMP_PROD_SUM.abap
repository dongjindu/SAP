************************************************************************
* Program Name      : ZAPP_ASMP_PROD_SUM
* Author            : Kim Chang Ryul
* Creation Date     : 07.15.2014
* Specifications    : AS&MP Production Log Summarization
* Development Request No :
* Addl Documentation:
* Description       : copied from ZIPP_ASMP_PIR
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  ZAPP_ASMP_PROD_SUM   NO STANDARD PAGE HEADING LINE-SIZE 132
                                                      MESSAGE-ID zmpp.


tables : ztpp_asmppr,mara,ZTPP_ASMP_SUM.
*
data: it_main like ztpp_asmppr   occurs 0 with header line.
*
data : begin of it_tab occurs 0.
        include structure ztpp_asmp_sum.
data : zmess(40)  TYPE C.
data : end of it_tab.

*----------------------------------------------------------------------*
* SELECTION-SCREEN.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME title text-b01.
PARAMETERS : p_werks      LIKE   t001w-werks OBLIGATORY MEMORY ID wrk.
SELECT-OPTIONS: S_RDATU   FOR ZTPP_ASMP_SUM-RDATU.
SELECTION-SCREEN END OF BLOCK b1.



*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM get_data.
  IF it_main[] IS INITIAL.
    MESSAGE i000 WITH 'No data'.
  ELSE.
    PERFORM save_process.
  ENDIF.


************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  perform data_write.

*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT .

* Memorory id WRK
  get parameter id 'WRK' field P_werks.

*-Shop Data
  CONCATENATE sy-datum+0(6) '01' INTO S_rdatu-low.
  S_rdatu-HIGH  = sy-datum.
  append S_rdatu.
ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

* into corresponding fields of table it_main
  refresh it_main.
  select WERKS MODEL LINE MATNR RDATU PQTY
     INTO CORRESPONDING FIELDS OF TABLE it_main
     from ztpp_asmppr
    where werks   = p_werks
      and rdatu  in S_RDATU
      and zresult = 'S'.
  sort it_main by WERKS MODEL LINE MATNR RDATU.

  refresh : it_tab.
  clear   : it_tab.
  loop at it_main.
    move-corresponding it_main  to it_tab.
    move: it_main-PQTY          to it_tab-menge,
          'EA'                  TO it_tab-meins,
          sy-uname              to it_tab-ernam,
          sy-datum              to it_tab-erdat,
          sy-uzeit              to it_tab-erzet,
          sy-uname              to it_tab-aenam,
          sy-datum              to it_tab-aedat,
          sy-uzeit              to it_taB-aezet.
    COLLECT it_tab.
  endloop.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_PROCESS .

  perform delete_save.


ENDFORM.                    " EXCUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DELETE_SHIP_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_SAVE.

* Delete data ztpp_asmp_sum : RDATU = shop date
  DELETE FROM ZTPP_ASMP_SUM
        WHERE RDATU IN S_RDATU.
*
* Insert summairzation : ztpp_asmp_sum.
  modify ztpp_asmp_sum from table it_tab.
  IF sy-subrc = 0.
    COMMIT WORK.
    loop at it_tab.
      it_tab-zmess = 'Data was updated successfully.'.
      modify it_tab.
    endloop.
  else.
    loop at it_tab.
      it_tab-zmess = 'Update failed.'.
      modify it_tab.
    endloop.
    ROLLBACK WORK.
  endif.

ENDFORM.                    " DELETE_SHIP_DATE
*&---------------------------------------------------------------------*
*&      Form  DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_WRITE .


  write :/(04) 'Plant',
          (04) 'Model code',
          (10) 'Line',
          (18) 'Material',
          (08) 'RP Date',
          (17) 'Quantity',
          (03) 'Unit',
          (40) 'Message'.

  loop at it_tab.
    write :/(04) it_tab-werks,
            (04) it_tab-model,
            (10) it_tab-line,
            (18) it_tab-matnr,
            (08) it_tab-rdatu,
            (17) it_tab-menge,
            (03) it_tab-meins,
            (40) it_tab-zmess.



  endloop.







ENDFORM.                    " DATA_WRITE
