* OUTPUT modules

*---------------------------------------------------------------------*
*       MODULE INIT_9870                                              *
*---------------------------------------------------------------------*
*       infotype specific initializations                             *
*---------------------------------------------------------------------*
MODULE init_9870 OUTPUT.
* replace with infotype specific coding
* set up to track the value of the grade list boxes.
  PERFORM change_listbox_grade USING p9870-track.

ENDMODULE.                    "INIT_9870 OUTPUT

* INPUT modules
