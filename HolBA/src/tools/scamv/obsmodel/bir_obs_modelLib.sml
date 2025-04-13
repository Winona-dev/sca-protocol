structure bir_obs_modelLib :> bir_obs_modelLib =
struct

open HolKernel boolLib liteLib simpLib Parse bossLib bslSyntax;

  (* error handling *)
  val libname  = "bir_obs_modelLib"
  val ERR      = Feedback.mk_HOL_ERR libname
  val wrap_exn = Feedback.wrap_exn libname

val shadow_begin_fencepost = beq (bconst64 41, bconst64 41);
val shadow_end_fencepost = beq (bconst64 42, bconst64 42);

fun problem_gen fname t msg = 
    raise ERR fname (msg ^ (term_to_string t));

local
    open bir_obs_modelTheory;
in

fun proginst_fun_gen obs_type prog =
  inst [Type`:'obs_type` |-> obs_type] prog;

structure bir_empty_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_empty ^mb ^t``));
end

structure bir_pc_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``(* add_obs_pc *) add_obs_pc_mem_bounds ^mb ^t``));
end

structure bir_arm8_mem_addr_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_mem_addr_armv8 ^mb ^t``));
end

structure bir_arm8_mem_addr_pc_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_mem_addr_pc_armv8 ^mb ^t``));
end

structure bir_arm8_mem_addr_pc_r0_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_mem_addr_pc_r0_armv8 ^mb ^t``));
end

structure bir_arm8_mem_addr_pc_lspc_model : OBS_MODEL =
struct
val obs_hol_type = ``:load_store_pc_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_mem_addr_pc_lspc_armv8 ^mb ^t``));
end

structure bir_arm8_cache_line_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_cache_line_tag_index_armv8 ^mb ^t``));
end

structure bir_arm8_cache_line_tag_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_cache_line_tag_armv8 ^mb ^t``));
end

structure bir_arm8_cache_line_index_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_cache_line_index_armv8 ^mb ^t``));
end

structure bir_arm8_cache_line_subset_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_cache_line_subset_armv8 ^mb ^t``));
end

structure bir_arm8_cache_line_subset_page_model : OBS_MODEL =
struct
val obs_hol_type = ``:bir_val_t``;
fun add_obs mb t en = rand (concl (EVAL ``add_obs_cache_line_subset_page_armv8 ^mb ^t``));
end

end (* local *)

local

(* ================================================ *)
open bir_programSyntax
open bir_program_labelsTheory
open bir_expSyntax
open bir_block_collectionLib;
open bir_cfgLib;
(* ================================================ *)

    type 'a stack = 'a list;
    val empty = [];
    val is_empty = null;
    fun push x s = x :: s;
    fun pop (x :: s) = (x,s);

    fun mk_key_from_address64 i addr = (mk_BL_Address o bir_immSyntax.mk_Imm64 o wordsSyntax.mk_word) (addr, Arbnum.fromInt i);

    fun get_bir_successors bb =
	let
	  fun get_bir_addr s =
	    if is_BLE_Label s then
                SOME (dest_BLE_Label s)
            else NONE
          val (_, _, bbes) = dest_bir_block bb;
	  val successor =
              if is_BStmt_Jmp bbes then
                let val s = dest_BStmt_Jmp bbes
                in [get_bir_addr s]
                end
	      else 
                if is_BStmt_CJmp bbes then
		  ((fn (_, t1, t2) => [get_bir_addr t1, get_bir_addr t2]) (dest_BStmt_CJmp bbes))
		else
		  [NONE]
	in successor end;

    fun get_cjmp_predecessors baddr blocks =
	let
	  val predecessors = List.filter
				 (fn bb =>
				     let
				       val (_,_,bbes) = dest_bir_block bb;
				       val jaddr = if is_BStmt_Jmp bbes then
						       let val bbesl = dest_BStmt_Jmp bbes;
						       in
							 if is_BLE_Label bbesl then
							   SOME (dest_BLE_Label bbesl)
							 else NONE
						       end
						   else
						     if is_BStmt_CJmp bbes then
						       let val (_,bbeslt1,_) = dest_BStmt_CJmp bbes;
						       in
							 if is_BLE_Label bbeslt1 then
							   SOME (dest_BLE_Label bbeslt1)
							 else NONE
						       end
						     else
						       NONE
				     in
				       isSome jaddr andalso identical (valOf jaddr) baddr
				     end) blocks;
	in
	  case predecessors of
	    [p] => p
	  | (p::ps) => raise ERR "get_cjmp_predecessors" "more than one predecessor"
	  | _ => raise ERR "get_cjmp_predecessors" "predecessor not found"
	end;

    (* single entry recursion, stop at first revisit or exit *)
    fun traverse_graph (g:cfg_graph) entry visited acc callstack =
	let
	    val n = lookup_block_dict_value (#CFGG_node_dict g) entry "traverse_graph" "n";
	    val targets = #CFGN_targets n;
	    val descr_o = #CFGN_hc_descr n;
	    val n_type  = #CFGN_type n;

	    val callstack = if cfg_nodetype_is_call n_type
			    then case n_type of
				     CFGNT_Call [e] => push (mk_BL_Address e) callstack
				   | _ => raise ERR "callstack" "more than one call address"
			    else callstack;

	    val acc_new = (if cfg_node_type_eq (n_type, CFGNT_CondJump) then [entry] else [])@acc;
	    val targets_to_visit = List.filter (fn x => List.all (fn y => not (identical x y)) visited) targets;
	in
	    if List.null targets andalso ((not o is_empty) callstack)
	    then
		let val (addr, rest) = pop callstack
		in
		List.foldr (fn (entry',(visited',acc')) => traverse_graph g entry' visited' acc' rest) 
		       (entry::visited, acc_new) 
		       ([addr]@targets_to_visit)
		end
	    else 
		List.foldr (fn (entry',(visited',acc')) => traverse_graph g entry' visited' acc' callstack) 
		       (entry::visited, acc_new) 
		       targets_to_visit
	end;

    fun traverse_graph_branch (g:cfg_graph) depth  entry visited acc =
	let
	    val n = lookup_block_dict_value (#CFGG_node_dict g) entry "traverse_graph" "n";
	    val targets = #CFGN_targets n;
	    val descr_o = #CFGN_hc_descr n;
	    val n_type  = #CFGN_type n;

	    val (targets_to_visit, acc_new) = 
		if (cfg_node_type_eq (n_type, CFGNT_CondJump)) orelse (depth = 0) 
		then ([], (if cfg_node_type_eq (n_type, CFGNT_CondJump) then [entry] else [])@acc)
		else (List.filter (fn x => List.all (fn y => not (identical x y)) visited) targets,
		      (if cfg_node_type_eq (n_type, CFGNT_CondJump) then [entry] else [])@acc)
	in

	    List.foldr (fn(entry',(visited',acc')) => traverse_graph_branch g (depth-1) entry' visited' acc') 
		       (entry::visited, acc_new) 
		       targets_to_visit
	end;

    (* given a branch, extract the statements of that branch as a list *)
    fun extract_branch_stmts depth branch bl_dict =
        let
          open listSyntax
          val dest_list_ignore_type = fst o dest_list;
          fun extract_stmts_from_lbl lbl =
              let open bir_programSyntax;
                  val block = Redblackmap.find (bl_dict, lbl) handle e => “<|bb_label := ^lbl;
		  bb_statements := ([] :bir_val_t bir_stmt_basic_t list );
		  bb_last_statement := BStmt_Halt (BExp_Const (Imm32 (0w :word32)))|>”
                  val (_, statements, _) = dest_bir_block block;
                  (* statements is a HOL list of BIR statements *)
              in statements end;
	  fun get_bir_successors bb =
	      let
		val (_, _, bbes) = dest_bir_block bb;
	        val successor =
		  if is_BStmt_Jmp bbes then
                    let val s = dest_BStmt_Jmp bbes
                    in if is_BLE_Label s then
			 SOME (dest_BLE_Label s)
                       else NONE
                    end
		  else 
                    NONE
	      in successor end;

          val first_block = Redblackmap.find (bl_dict, branch) handle e => “<|bb_label := ^branch;
		  bb_statements := ([] :bir_val_t bir_stmt_basic_t list );
		  bb_last_statement := BStmt_Halt (BExp_Const (Imm32 (0w :word32)))|>”

          fun collect_blocks 0 block = [block]
            | collect_blocks n block =
              case get_bir_successors block of
                  NONE => [block]
                | SOME lbl =>
                  let val b = Redblackmap.find (bl_dict, lbl) handle e => “<|bb_label := ^lbl;
		  bb_statements := ([] :bir_val_t bir_stmt_basic_t list );
		  bb_last_statement := BStmt_Halt (BExp_Const (Imm32 (0w :word32)))|>”
                  in
                    block :: collect_blocks (n-1) b
                  end;
          fun get_block_label block =
              let val (lbl,_,_) = dest_bir_block block
              in (rand o concl) (EVAL lbl) end;

          val blocks = collect_blocks (depth-1) first_block;
          val branch_labels = List.map get_block_label blocks;
          val stmts = List.map (dest_list_ignore_type o extract_stmts_from_lbl) branch_labels;
        in
          List.concat stmts
        end;

  fun update_node_guess_type_call bl_dict fun_entry_lbl_tms (n:cfg_node) =
      if not (cfg_node_type_eq (#CFGN_type n, CFGNT_Jump))
      then NONE else
      let
	  val lbl_tm = #CFGN_lbl_tm n;
	  val targets = #CFGN_targets n;
	  val descr_o = #CFGN_hc_descr n;

      in case targets of
	 [target] =>
	 let
	  val bl =
		    case lookup_block_dict bl_dict lbl_tm of
		       SOME x => x
		     | NONE => raise ERR "update_node_guess_type_call"
					 ("cannot find label " ^ (term_to_string lbl_tm));


	  val isCall_to_entry = (List.exists (fn x => identical x target) fun_entry_lbl_tms);
	  val _ = if isCall_to_entry then ()
	      else raise ERR "update_node_guess_type_call"
			     ("something in call detection is unexpected: " ^ (term_to_string lbl_tm));

	  val (_, bbs, _) = dest_bir_block bl;
	  val stmts = (fst o listSyntax.dest_list) bbs;

          val new_type =
	      (* change the type if it is an assignment to the link register *)
	      if List.exists is_BStmt_Assign stmts then
		  let
		    val assign = List.filter is_BStmt_Assign stmts;
		    val (var, exp) = dest_BStmt_Assign (hd assign);
		    val name = (fst o bir_envSyntax.dest_BVar_string) var;
		  in
		    if (name = "R30" andalso is_BExp_Const exp)
		    then CFGNT_Call [dest_BExp_Const exp]
		    else #CFGN_type n
		  end
	      else #CFGN_type n


	  val new_n =
	      { CFGN_lbl_tm   = #CFGN_lbl_tm n,
		CFGN_hc_descr = #CFGN_hc_descr n,
		CFGN_targets  = #CFGN_targets n,
		CFGN_type     = new_type
	      } : cfg_node;
	 in
	   SOME new_n
	 end
      | _ => NONE end;

    fun nub_with eq [] = []
      | nub_with eq (x::xs) = x::(nub_with eq (List.filter (fn y => not (eq (y, x))) xs))

    fun bir_free_vars exp =
	let
	    open stringSyntax;
	    fun var_to_str v =
		let val (name,_) = dest_var v
		in
		    fromMLstring name
		end
	    val fvs =
	        if is_comb exp then
		    let val (con,args) = strip_comb exp
		    in
		        if identical con ``BExp_MemConst``
		        then [var_to_str (List.nth(args, 2))]
		        else if identical con ``BExp_Den``
		        then
		            let val v = case strip_comb (hd args) of
				            (_,v::_) => v
				          | _ => raise ERR "bir_free_vars" "not expected"
		            in
			        [v]
		            end
		        else
		            List.concat (map bir_free_vars args)
		    end
	        else []
	in
	    nub_with (fn (x,y) => identical x y) fvs
	end;

    fun bir_free_vars_stmt_b stmt_b =
	      let
	        open stringSyntax;
          open bir_envSyntax;
          open bir_programSyntax;
          val fvs =
		          if is_BStmt_Assign stmt_b
              then
                let val (var,exp) = dest_BStmt_Assign stmt_b
		            in
                  (fst (dest_BVar var))::(bir_free_vars exp)
                end
              else
                bir_free_vars stmt_b;
        in
          nub_with (fn (x,y) => identical x y) fvs
        end;

    fun primed_term t =
	      let open stringSyntax numSyntax;
	          fun primed_subst tm =
		            List.map (fn v =>
			                       let val vp = lift_string string_ty (fromHOLstring v ^ "*")
			                       in ``^v`` |-> ``^vp`` end)
			                   (bir_free_vars_stmt_b tm)
        in
          List.foldl (fn (record, tm) => subst[#redex record |-> #residue record] tm) t (primed_subst t)
        end

    fun const_obs t =
        if is_BStmt_Observe t
        then let open listSyntax;
                 val (_,_,obs_list_tm,_) = dest_BStmt_Observe t;
                 val (obs_list,_) = dest_list obs_list_tm;
             in
               length obs_list = 1 andalso is_BExp_Const (hd obs_list)
             end
        else false

    fun mk_preamble stmts =
        let open stringSyntax;
            val free_vars = nub_with (uncurry identical)
                                     (List.concat (map bir_free_vars_stmt_b stmts));
            fun star_string str =
                  lift_string string_ty (fromHOLstring str ^ "*")
            fun mk_assignment var =
                let val var_type =
                        if fromHOLstring var = "MEM"
                        then “BType_Mem Bit64 Bit8”
                        else “BType_Imm Bit64”
                    val var_star_tm = “BVar ^(star_string var) ^var_type”
                in inst [Type.alpha |-> Type`:bir_val_t`]
                        (mk_BStmt_Assign (var_star_tm, “BExp_Den (BVar ^var ^var_type)”))
                end;
            val fencepost_begin = if null free_vars
                                  then []
                                  else [inst [Type.alpha |-> Type‘:bir_val_t’]
                                             (bassert shadow_begin_fencepost)];
        in
          fencepost_begin @ List.map mk_assignment free_vars
        end;

    (* generate shadow branch for a given branch (to be inserted in the other) *)
    fun gen_shadow_branch obs_fun depth dict branch =
        let
          open listSyntax;
          open pairSyntax;
          open bir_valuesSyntax; 
          val stmts = extract_branch_stmts depth branch dict;
          val preamble = mk_preamble stmts;
          (* add stars to every free variable *)
          val stmts_starred = map primed_term stmts;
          (* remove constant observations (pc observations) *)
          val stmts_without_pc = filter (not o const_obs) stmts_starred;
          (* tag observations as refinements, as per obs_fun
             NB. Refinement will not work unless obs_fun tags
                 some observations with 1 *)
          val stmts_obs_tagged = obs_fun stmts_without_pc;
          val fencepost_assertion = if null stmts_obs_tagged
                                    then []
                                    else [inst [Type.alpha |-> bir_val_t_ty] (bassert shadow_end_fencepost)];
        in
          preamble @ stmts_obs_tagged @ fencepost_assertion
        end

    (* generate shadow branches for a given cjmp *)
    fun add_shadow_branches obs_fun depth dict (left_branch, right_branch) prog =
	let
	    open listSyntax
	    open pairSyntax
      fun to_stmt_list xs = mk_list(xs, “:bir_val_t bir_stmt_basic_t”);
      val gen_shadow = gen_shadow_branch obs_fun depth dict;
      val left_shadow =  to_stmt_list (gen_shadow left_branch)
      val right_shadow = to_stmt_list (gen_shadow right_branch);
      fun prepend_block (itm, p) =
          (rhs o concl o EVAL)``prepend_obs_in_bir_prog_block ^itm ^p``
	in
	    foldl prepend_block prog
		  [“(^left_branch, ^right_shadow)”, “(^right_branch, ^left_shadow)”]
	end

    fun obs_refined n stm =
        if is_BStmt_Observe stm
        then let open numSyntax;
                 val (obs_id,cond,obs_list_tm,f) = dest_BStmt_Observe stm;
             in
               mk_BStmt_Observe (term_of_int n, cond, obs_list_tm, f)
             end
        else stm

    val obs_all_refined = List.map (obs_refined 1);
    fun obs_all_refined_but_first stmts =
        let fun go [] = []
              | go (stmt::stmts) =
                if is_BStmt_Observe stmt
                then obs_refined 0 stmt :: obs_all_refined stmts
                else stmt :: go stmts
        in
          go stmts
        end

    open bir_expSyntax bir_immSyntax bir_envSyntax bir_exp_immSyntax bir_exp_memSyntax;
    open bir_valuesSyntax;
    open wordsSyntax;

    fun swap_cnd_cjmp_shadow_blocks shadow_blocks cjmp_addrs =
	let val cjmp_to_swap = List.map
				 (fn addr =>
				   ("0x"^(Arbnum.toHexString o Arbnum.fromInt) addr)^"*")
				 cjmp_addrs;
	  fun check_block bb =
	      let val (bbl,bbs,bbes) = dest_bir_block bb;
		fun change_cnd () =
		    let
		      val bbla = dest_BL_Label_string bbl;
		    in
		      if List.exists (fn bba => bba=bbla) cjmp_to_swap
		      then
			let val (cnd,lblet,lblef) = if is_BStmt_CJmp bbes
						    then dest_BStmt_CJmp bbes
						    else raise ERR "swap_cnd_cjmp_shadow_blocks" "defined address is not a conditional branch"
			    val fixed_cnd = bnot cnd;
			in
			  mk_bir_block (bbl,bbs,mk_BStmt_CJmp (fixed_cnd,lblet,lblef))
			end
		      else bb
		    end
	      in
		if is_BL_Label bbl
		then
		  change_cnd ()
		else raise ERR "swap_cnd_cjmp_shadow_blocks" "wrong shadow address type"
	      end

	in
	  List.map check_block shadow_blocks
	end

    fun primed_word_literal wv =
	let
	  open stringSyntax numSyntax;
	  val vstr =
	      if is_word_literal wv then
		  (Arbnum.toHexString o dest_word_literal) wv
	      else raise ERR "primed_word_literal" "can only handle word literals";
        in
	  (lift_string string_ty ("0x" ^ vstr ^ "*"))
	end

    fun mk_shadow_addr exp =
	let
	  fun problem exp msg = problem_gen "mk_shadow_addr" exp msg;
	in
	  if is_BL_Address exp then
	    let
	      val (sz, wv) = (gen_dest_Imm o dest_BL_Address) exp;
	      val primed_wv = primed_word_literal wv;
            in
	      mk_BL_Label primed_wv
            end
	  else
	    problem exp "the expression is not a bir address: "
	end

    fun mk_shadow_label lbl =
	let
	  fun problem exp msg = problem_gen "mk_shadow_label" exp msg;
	in
	  if is_BL_Address lbl then
	    mk_shadow_addr lbl
	  else if is_BL_Label lbl then
	    let
	      open stringSyntax numSyntax;
	      val str = (dest_BL_Label_string) lbl;
	    in
	      mk_BL_Label (lift_string string_ty (str ^ "*"))
	    end
	  else
	    problem lbl "unknown bir label: "
	end

    fun mk_shadow_blabelexp exp =
	let
	  fun problem exp msg = problem_gen "mk_shadow_blabelexp" exp msg;
	in
	  if is_BLE_Label exp then
	    (mk_BLE_Label o mk_shadow_addr) (dest_BLE_Label exp)
	  else if is_BLE_Exp exp then
	    primed_term exp
	  else
	    problem exp "unknown bir expression: "
	end

    fun mk_shadow_estmt estmt =
	if is_BStmt_Halt estmt then estmt
	  (*let
	    val e = dest_BStmt_Halt estmt;
	    val v = dest_BExp_Const e;
	    val addr = mk_BL_Address v;
	  in
	    mk_BStmt_Jmp ((mk_BLE_Label o mk_shadow_addr) addr)
	  end*)
	else if is_BStmt_Jmp estmt then
	  let
	    val e = dest_BStmt_Jmp estmt;
	  in
	    mk_BStmt_Jmp (mk_shadow_blabelexp e)
	  end
	else if is_BStmt_CJmp estmt then
	  let
	    val (cnd_tm, lblet_tm, lblef_tm) = dest_BStmt_CJmp estmt;
	    val reverse_cnd = bnot (primed_term cnd_tm);
	  in
	    mk_BStmt_CJmp (reverse_cnd, mk_shadow_blabelexp lblet_tm, mk_shadow_blabelexp lblef_tm)
	  end
	else
	    raise ERR "mk_shadow_estmt" "unknown bir end statement"

    fun mk_shadow_block block =
	let
	  open listSyntax;
	  val (tm_lbl, tm_stmts, tm_last_stmt) = dest_bir_block block;
	  val (stmt_list, obs_ty) = (dest_list) tm_stmts;
	  val lbl_shadow =  mk_shadow_label tm_lbl;
	  val stmts_starred = map primed_term stmt_list;
	  val estmt_shadow = mk_shadow_estmt tm_last_stmt;
	in
	  mk_bir_block (lbl_shadow, mk_list (stmts_starred, obs_ty), estmt_shadow)
	end

    fun mk_shadow_target block pt =
	let
	  fun problem exp msg = problem_gen "mk_shadow_target" exp msg;
	  val (tm_lbl, tm_stmts, tm_last_stmt) = dest_bir_block block;
	  val estmt_shadow =
	    if is_BStmt_Jmp tm_last_stmt then
	      if identical tm_lbl pt then
		let
		  val e = dest_BStmt_Jmp tm_last_stmt
		in
		  mk_BStmt_Jmp (mk_shadow_blabelexp e)
		end
	      else
		tm_last_stmt
	    else
	    if is_BStmt_CJmp tm_last_stmt then
	      let
		val (cnd_tm, lblet_tm, lblef_tm) = dest_BStmt_CJmp tm_last_stmt;
	      in
		if identical tm_lbl pt then
		  (* make shadow address only for the true target of the predecessor block *)
		  mk_BStmt_CJmp (cnd_tm, mk_shadow_blabelexp lblet_tm, (* mk_shadow_blabelexp *) lblef_tm)
		else
		  tm_last_stmt
	      end
	    else
	    if is_BStmt_Halt tm_last_stmt then
	      tm_last_stmt
	    else
	      problem tm_last_stmt "unknown bir end statement: "
	in
	  mk_bir_block (tm_lbl, tm_stmts, estmt_shadow)
	end

    fun extract_blocks depth branch bl_dict =
	let
	  fun get_block_from_dict baddr =
	      let
		fun problem exp msg = problem_gen "extract_blocks::get_block_from_dict" exp msg;
	      in
		  Redblackmap.find (bl_dict, baddr)
		  handle e => “<|bb_label := ^baddr;
		  bb_statements := ([] :bir_val_t bir_stmt_basic_t list );
		  bb_last_statement := BStmt_Halt (BExp_Const (Imm32 (0w :word32)))|>”
	      end
	  val shadow_block_fun = (mk_shadow_block o (snd o dest_eq o concl o EVAL))
	  fun fix_jmp_back bb =
	    let
	      open listSyntax;
	      val (bbl, bbs, bbes) = dest_bir_block bb;
	      val (stmts, ty) = dest_list bbs;
	      val fencepost_assertion = [inst [Type.alpha |-> bir_val_t_ty] (bassert shadow_end_fencepost)];
	      val estmt =
		  let
		    fun problem exp msg = problem_gen "extract_blocks::fix_jmp_back" exp msg;
		  in
		    if is_BStmt_Jmp bbes then
		      mk_BStmt_Jmp (mk_BLE_Label branch)
		    else if is_BStmt_Halt bbes then
		      mk_BStmt_Jmp (mk_BLE_Label branch)
		    else
		      problem bbes "can't handle the end statement: "
		  end
	    in
              mk_bir_block (bbl, mk_list (stmts@fencepost_assertion, ty), estmt)
	    end

          val first_block = get_block_from_dict branch;

          fun collect_blocks 0 block = [(fix_jmp_back o shadow_block_fun) block]
            | collect_blocks n block =
              case get_bir_successors block of
                  [NONE] => [(fix_jmp_back o shadow_block_fun) block]
                | [SOME lbl] =>
                  let val b = get_block_from_dict lbl
                  in
                    (shadow_block_fun block) :: collect_blocks (n-1) b
                  end
		| [SOME lbl1, SOME lbl2] =>
		  let
		    val b1 = get_block_from_dict lbl1
		    val b2 = get_block_from_dict lbl2
                  in
                    ((shadow_block_fun block) :: collect_blocks (n-1) b2 @ collect_blocks (n-1) b1)
                  end
		| _ => raise ERR "extract_blocks::collect_blocks" "unknown bir successor type"
        in
          collect_blocks (depth-1) first_block
        end

    fun add_obs_refined obs_fun sblocks =
	let
	  open listSyntax;
	  fun add_obs_reined_to_block bb =
	      let
		val (bbl,bbs,bbes) = dest_bir_block bb;
		val (stmt_list, obs_ty) = (dest_list) bbs;
		val stmts_without_pc = filter (not o const_obs) stmt_list;
		val stmts_obs_tagged = obs_fun stmts_without_pc;
	      in
		mk_bir_block (bbl, mk_list(stmts_obs_tagged, obs_ty), bbes)
	      end
	in
	  List.map add_obs_reined_to_block sblocks
	end

    fun save_shadow_state sblocks =
        let
	  open listSyntax;
	  open stringSyntax;
          val free_vars = nub_with (uncurry identical)
                                   (List.concat (map bir_free_vars sblocks));
          fun not_starred_var var =
	      let val var_char = String.explode (fromHOLstring var);
	      in
		  if last var_char = #"*" then
		      String.implode (List.take (var_char, (List.length var_char)-1))
		  else
		      raise ERR "save_shadow_state::not_starred_var" "a starred variable was expected"
	      end
          fun mk_assignment var =
              let val var_type = (* Note: also bring the variable type from bir_free_vars? *)
                      if fromHOLstring var = "MEM*"
                      then “BType_Mem Bit64 Bit8”
                      else if String.isSubstring "ProcState" (fromHOLstring var)
		      then “BType_Imm Bit1”
		      else “BType_Imm Bit64”
                  val var_tm = bden (bvar (not_starred_var var) var_type)
              in inst [Type.alpha |-> Type`:bir_val_t`]
                      (mk_BStmt_Assign (“BVar ^var ^var_type”, var_tm))
              end;
          val fencepost_begin = if null free_vars
                                then []
                                else [inst [Type.alpha |-> Type‘:bir_val_t’]
                                           (bassert shadow_begin_fencepost)];
	  val preamble = fencepost_begin @ List.map mk_assignment free_vars;
        in
          (let
             val (bbl,bbs,bbes) = dest_bir_block (hd sblocks);
	     val (stmts, _) = dest_list bbs;
	   in
	     mk_bir_block (bbl, mk_list (preamble@stmts, ``:bir_val_t bir_stmt_basic_t``), bbes)
	   end
	  )::(tl sblocks)
        end;

    fun mk_shadow_prog obs_fun bprog bl_dict target ptarget depth =
	let
	  open listSyntax;
	  val (blocks, obs_ty) = (dest_list o dest_BirProgram) bprog;

	  val shadow_blocks = nub_with (fn (x,y) => identical x y) (extract_blocks depth target bl_dict);
	  val shadow_blocks_w_refobs = add_obs_refined obs_fun shadow_blocks;
	  val complete_shadow_blocks = save_shadow_state shadow_blocks_w_refobs;

	  val link_blocks = map (fn b => mk_shadow_target ((rand o concl) (EVAL b)) ptarget) blocks;

	  (* val list_cjmp_to_swap = [0x4000ac, 0x4000d8, 0x4000ec]; *)
	  (* val fixed_blocks = swap_cnd_cjmp_shadow_blocks complete_shadow_blocks list_cjmp_to_swap; *)
	in
	  mk_BirProgram (mk_list (link_blocks@complete_shadow_blocks, obs_ty))
	end

 fun branch_instrumentation obs_fun prog entry depth =
     let (* build the dictionaries using the library under test *)
	 open bir_programSyntax listSyntax bir_program_labelsSyntax;
	 val bl_dict = gen_block_dict prog;
	 val blocks = fst (dest_list (dest_BirProgram prog));

	 fun splitAtElement (_, []) = []
	   | splitAtElement (x, h::t) =
	     let
		 val (lbl,_,_) = dest_bir_block h;
		 val addr = ((dest_word_literal o snd o gen_dest_Imm o dest_BL_Address) lbl);
	     in
		 if addr = x then
		     (h::t) 
		 else
		     splitAtElement (x, t)
	     end

	 val entry_block_list = splitAtElement (entry, blocks);
	     
	 fun map_pair f (x,y) = (f x, f y);

       fun get_targets block =
           let val (bbl, _, bbes) = dest_bir_block block
           in if is_BStmt_CJmp bbes
              then let val (_,left,right) = dest_BStmt_CJmp bbes
                   in SOME (left,right)
                   end
              else NONE
           end;
       fun block_filter block =
           case get_targets block of
                   SOME (left,right) =>
                   is_BLE_Label left
                   andalso is_BLE_Label right
                   andalso not (identical left right)
                 | NONE => false;
       fun get_block_label block =
           let val (lbl,_,_) = dest_bir_block block
           in (rand o concl) (EVAL lbl) end;
       val targets = List.map get_block_label (List.filter block_filter entry_block_list);
       val (target, pred_target) =
	   (* NOTE: now we target the first conditional branch *)
	   case targets of
	       (t::_) => (t, get_block_label (get_cjmp_predecessors t blocks))
	     | _ => raise ERR "branch_instrumentation" "no target branch found";
     in
	 if depth < 1
	 then
	     raise ERR "branch_instrumentation" "the depth cannot be less than 1"
	 else
	     mk_shadow_prog obs_fun prog bl_dict target pred_target depth
     end
         
 fun jmp_to_cjmp t = (rand o concl) (EVAL “jmp_to_cjmp_prog ^t”);

in

  (* Exmaple usage: inputs are lifted program with intial observation and depth of execution      *)
  (* branch_instrumentation_obs lifted_prog_w_obs 3; *)

  structure bir_arm8_cache_speculation_model : OBS_MODEL =
    struct
      val obs_hol_type = ``:bir_val_t``;
      val pipeline_depth = 25;
      fun add_obs mb t en =
        branch_instrumentation obs_all_refined (bir_arm8_mem_addr_pc_model.add_obs mb t en) en pipeline_depth;
    end;

  structure bir_arm8_cache_speculation_idx_model : OBS_MODEL =
    struct
      val obs_hol_type = ``:bir_val_t``;
      val pipeline_depth = 25;
      fun add_obs mb t en =
        branch_instrumentation obs_all_refined (bir_arm8_mem_addr_pc_r0_model.add_obs mb t en) en pipeline_depth;
    end;

  structure bir_arm8_cache_speculation_first_model : OBS_MODEL =
  struct
  val obs_hol_type = ``:bir_val_t``;
  val pipeline_depth = 25;
  fun add_obs mb t en =
      branch_instrumentation obs_all_refined_but_first (bir_arm8_mem_addr_pc_model.add_obs mb t en) en pipeline_depth;
  end;

  structure bir_arm8_cache_straight_line_model : OBS_MODEL =
  struct
  val obs_hol_type = ``:bir_val_t``;
  val pipeline_depth = 25;
  fun add_obs mb t en =
      let val obs_term = bir_arm8_mem_addr_pc_model.add_obs mb t en;
          val jmp_to_cjmp_term = jmp_to_cjmp obs_term;
      in
        branch_instrumentation obs_all_refined jmp_to_cjmp_term en pipeline_depth
      end;
  end;
  
end (* local *)


fun get_obs_model id =
  let
    val obs_hol_type =
             if id = "mem_address_pc" then
	  bir_arm8_mem_addr_pc_model.obs_hol_type
        else if id = "mem_address_pc_lspc" then
	  bir_arm8_mem_addr_pc_lspc_model.obs_hol_type
        else if id = "cache_tag_index" then
          bir_arm8_cache_line_model.obs_hol_type
        else if id = "cache_tag_only" then
          bir_arm8_cache_line_tag_model.obs_hol_type
        else if id = "cache_index_only" then
          bir_arm8_cache_line_index_model.obs_hol_type
        else if id = "cache_tag_index_part" then
          bir_arm8_cache_line_subset_model.obs_hol_type
        else if id = "cache_tag_index_part_page" then
          bir_arm8_cache_line_subset_page_model.obs_hol_type
        else if id = "cache_speculation" then
               bir_arm8_cache_speculation_model.obs_hol_type
	else if id = "cache_speculation_idx" then
               bir_arm8_cache_speculation_idx_model.obs_hol_type
	else if id = "cache_speculation_first" then
               bir_arm8_cache_speculation_first_model.obs_hol_type
        else if id = "cache_straightline" then
               bir_arm8_cache_straight_line_model.obs_hol_type
	else if id = "pc_only" then
               bir_pc_model.obs_hol_type
	else if id = "empty" then
               bir_empty_model.obs_hol_type
	else
            raise ERR "get_obs_model" ("unknown obs_model selected: " ^ id);

    val add_obs =
             if id = "mem_address_pc" then
          bir_arm8_mem_addr_pc_model.add_obs
        else if id = "mem_address_pc_lspc" then
          bir_arm8_mem_addr_pc_lspc_model.add_obs
        else if id = "cache_tag_index" then
          bir_arm8_cache_line_model.add_obs
        else if id = "cache_tag_only" then
          bir_arm8_cache_line_tag_model.add_obs
        else if id = "cache_index_only" then
          bir_arm8_cache_line_index_model.add_obs
        else if id = "cache_tag_index_part" then
          bir_arm8_cache_line_subset_model.add_obs
        else if id = "cache_tag_index_part_page" then
          bir_arm8_cache_line_subset_page_model.add_obs
        else if id = "cache_speculation" then
               bir_arm8_cache_speculation_model.add_obs
	else if id = "cache_speculation_idx" then
               bir_arm8_cache_speculation_idx_model.add_obs
	else if id = "cache_speculation_first" then
               bir_arm8_cache_speculation_first_model.add_obs
        else if id = "cache_straightline" then
               bir_arm8_cache_straight_line_model.add_obs
	else if id = "pc_only" then
               bir_pc_model.add_obs
	else if id = "empty" then
               bir_empty_model.add_obs
	else
          raise ERR "get_obs_model" ("unknown obs_model selected: " ^ id);
  in
    { id = id,
      obs_hol_type = obs_hol_type,
      add_obs = add_obs }
  end;

end (* struct *)
