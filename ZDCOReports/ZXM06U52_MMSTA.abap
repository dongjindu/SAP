*----------------------------------------------------------------------*
*   INCLUDE ZXM06U52_MMSTA                                             *
*----------------------------------------------------------------------*
*DATA: l_mmsta LIKE marc-mmsta,
*      l_mmstd LIKE marc-mmstd,
*      l_msgty LIKE cmimsg-msgty.
*
*SELECT SINGLE mmsta mmstd
*   INTO (l_mmsta, l_mmstd) FROM marc
*   WHERE matnr = i_bqpim-matnr
*     AND werks = i_bqpim-werks.
*
*IF l_mmstd IS INITIAL
*OR l_mmstd >= i_bqpim-nedat.
*  CLEAR l_msgty.
*  CASE l_mmsta.
*    WHEN '14' OR '13'.
*      l_msgty = 'E'.
*    WHEN space.
*      l_msgty = 'W'.
*  ENDCASE.
*
*  IF NOT l_msgty IS INITIAL.
*    CALL FUNCTION 'CM_F_MESSAGE'
*         EXPORTING
*              arbgb            = 'ZMCO'
*              msgnr            = '000'
*              msgty            = l_msgty
*              msgv1            = i_bqpim-matnr
*              msgv2            = i_bqpim-werks
*              msgv3            = l_mmsta
*           msgv4            = 'This material status is not for costing'
*              object_dependent = 'X'
*         EXCEPTIONS
*              OTHERS           = 1.
*    IF sy-subrc <> 0.
*      MESSAGE w000(zmco) WITH i_bqpim-matnr i_bqpim-werks l_mmsta
*              'This material status is not for costing'.
*    ENDIF.
*  ENDIF.
*ENDIF.
