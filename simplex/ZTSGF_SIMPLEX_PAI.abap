*&---------------------------------------------------------------------*
*&  Include           ZTSGF_SIMPLEX_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.

  case sy-ucomm.
    when 'BACK' or 'CANC' or 'EXIT'.
      set screen 0.
      leave screen.
    when 'NEXT'.
      call screen 9010.
  endcase.

endmodule.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9010 input.

  case sy-ucomm.
    when 'BACK' or 'CANC' or 'EXIT'.
      set screen 0.
      leave screen.
    when 'NEXT'.
      perform zf_simplex_calculation.
      call screen 9020.
  endcase.

endmodule.                 " USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9020 input.

  case sy-ucomm.
    when 'BACK' or 'CANC' or 'EXIT'.
      set screen 0.
      leave screen.
  endcase.

endmodule.                 " USER_COMMAND_9030  INPUT
