*&---------------------------------------------------------------------*
*&  Include           ZTSGF_ALV_SPLIT_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
module user_command_9000 input.

  case sy-ucomm.
    when c_ucomm-back or c_ucomm-canc or c_ucomm-exit.
      set screen 0.
      leave screen.
  endcase.

endmodule.                 " USER_COMMAND_9010  INPUT
