open HolKernel Parse

open binariesLib;
open binariesCfgLib;
open binariesMemLib;

open commonBalrobScriptLib;

val entry_labels = ["motor_prep_input",
                    "__lesf2",
                    "__clzsi2",
                    "__aeabi_f2iz",
                    "pid_msg_write",
                    "timer_read"];

open binariesCfgVizLib;
open binariesDefsLib;

val _ = show_call_graph ();

(*
val entry_label = List.nth (entry_labels, 0);
val _ = show_cfg_fun true  bl_dict_ n_dict entry_label;
*)

val _ = show_cfg_fun false  bl_dict_ n_dict "imu_handler_pid_entry";

(*
fun print_option pf NONE     = print "NONE"
  | print_option pf (SOME x) = (print "SOME ("; pf x; print ")");
*)

