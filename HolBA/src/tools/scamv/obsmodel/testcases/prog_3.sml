(* ========================= prog_3 - spectre_v1 =========================== *)

(*
=================================
	cmp x1, x2
	b.hs #0x10
	ldr x4, [x3, x1]
	lsl x4, x4, #0x1
	ldr x6, [x5, x4]
=================================
*)

val prog_3 = ``
BirProgram
       [<|bb_label :=
            BL_Address_HC (Imm64 (0w :word64)) "EB02003F (cmp x1, x2)";
          bb_statements :=
            [(BStmt_Assign (BVar "ProcState_C" BType_Bool)
                (BExp_nzcv_SUB_C (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                   (BExp_Den (BVar "R2" (BType_Imm Bit64)))) :
              bir_val_t bir_stmt_basic_t);
             (BStmt_Assign (BVar "ProcState_N" BType_Bool)
                (BExp_nzcv_SUB_N Bit64
                   (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                   (BExp_Den (BVar "R2" (BType_Imm Bit64)))) :
              bir_val_t bir_stmt_basic_t);
             (BStmt_Assign (BVar "ProcState_V" BType_Bool)
                (BExp_nzcv_SUB_V Bit64
                   (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                   (BExp_Den (BVar "R2" (BType_Imm Bit64)))) :
              bir_val_t bir_stmt_basic_t);
             (BStmt_Assign (BVar "ProcState_Z" BType_Bool)
                (BExp_nzcv_SUB_Z (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                   (BExp_Den (BVar "R2" (BType_Imm Bit64)))) :
              bir_val_t bir_stmt_basic_t)];
          bb_last_statement :=
            BStmt_Jmp (BLE_Label (BL_Address (Imm64 (4w :word64))))|>;
        <|bb_label :=
            BL_Address_HC (Imm64 (4w :word64))
              "54000082 (b.cs 14 <.text+0x14>  // b.hs, b.nlast)";
          bb_statements := ([] :bir_val_t bir_stmt_basic_t list );
          bb_last_statement :=
            BStmt_CJmp (BExp_Den (BVar "ProcState_C" BType_Bool))
              (BLE_Label (BL_Address (Imm64 (20w :word64))))
              (BLE_Label (BL_Address (Imm64 (8w :word64))))|>;
        <|bb_label :=
            BL_Address_HC (Imm64 (8w :word64)) "F8616864 (ldr x4, [x3, x1])";
          bb_statements :=
            [(BStmt_Assert
                (BExp_Aligned Bit64 (3 :num)
                   (BExp_BinExp BIExp_Plus
                      (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                      (BExp_Den (BVar "R3" (BType_Imm Bit64))))) :
              bir_val_t bir_stmt_basic_t);
             (BStmt_Assign (BVar "R4" (BType_Imm Bit64))
                (BExp_Load (BExp_Den (BVar "MEM" (BType_Mem Bit64 Bit8)))
                   (BExp_BinExp BIExp_Plus
                      (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                      (BExp_Den (BVar "R3" (BType_Imm Bit64))))
                   BEnd_LittleEndian Bit64) :bir_val_t bir_stmt_basic_t)];
          bb_last_statement :=
            BStmt_Jmp (BLE_Label (BL_Address (Imm64 (12w :word64))))|>;
        <|bb_label :=
            BL_Address_HC (Imm64 (12w :word64)) "D37FF884 (lsl x4, x4, #1)";
          bb_statements :=
            [(BStmt_Assign (BVar "R4" (BType_Imm Bit64))
                (BExp_BinExp BIExp_And
                   (BExp_Const (Imm64 (18446744073709551615w :word64)))
                   (BExp_BinExp BIExp_LeftShift
                      (BExp_Den (BVar "R4" (BType_Imm Bit64)))
                      (BExp_Const (Imm64 (1w :word64))))) :
              bir_val_t bir_stmt_basic_t)];
          bb_last_statement :=
            BStmt_Jmp (BLE_Label (BL_Address (Imm64 (16w :word64))))|>;
        <|bb_label :=
            BL_Address_HC (Imm64 (16w :word64)) "F86468A6 (ldr x6, [x5, x4])";
          bb_statements :=
            [(BStmt_Assert
                (BExp_Aligned Bit64 (3 :num)
                   (BExp_BinExp BIExp_Plus
                      (BExp_Den (BVar "R5" (BType_Imm Bit64)))
                      (BExp_Den (BVar "R4" (BType_Imm Bit64))))) :
              bir_val_t bir_stmt_basic_t);
             (BStmt_Assign (BVar "R6" (BType_Imm Bit64))
                (BExp_Load (BExp_Den (BVar "MEM" (BType_Mem Bit64 Bit8)))
                   (BExp_BinExp BIExp_Plus
                      (BExp_Den (BVar "R5" (BType_Imm Bit64)))
                      (BExp_Den (BVar "R4" (BType_Imm Bit64))))
                   BEnd_LittleEndian Bit64) :bir_val_t bir_stmt_basic_t)];
          bb_last_statement :=
            BStmt_Jmp (BLE_Label (BL_Address (Imm64 (20w :word64))))|>;
        <|bb_label := BL_Address (Imm64 (20w :word64));
          bb_statements := ([] :bir_val_t bir_stmt_basic_t list );
          bb_last_statement := BStmt_Halt (BExp_Const (Imm32 (0w :word32)))|>]
:bir_val_t bir_program_t
``;

val prog_3_mem_address_pc = ``
F
``;

val prog_3_cache_tag_index = ``
F
``;

val prog_3_cache_tag_only = ``
F
``;

val prog_3_cache_index_only = ``
F
``;

val prog_3_cache_tag_index_part = ``
F
``;

val prog_3_cache_tag_index_part_page = ``
F
``;

val prog_3_cache_speculation = ``
BirProgram
      [<|bb_label := BL_Address (Imm64 0w);
         bb_statements :=
           [BStmt_Observe 0 (BExp_Const (Imm1 1w)) [BExp_Const (Imm64 0w)] HD;
            BStmt_Assign (BVar "ProcState_C" (BType_Imm Bit1))
              (BExp_BinPred BIExp_LessOrEqual
                 (BExp_Den (BVar "R2" (BType_Imm Bit64)))
                 (BExp_Den (BVar "R1" (BType_Imm Bit64))));
            BStmt_Assign (BVar "ProcState_N" (BType_Imm Bit1))
              (BExp_BinPred BIExp_SignedLessThan
                 (BExp_BinExp BIExp_Minus
                    (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                    (BExp_Den (BVar "R2" (BType_Imm Bit64))))
                 (BExp_Const (Imm64 0w)));
            BStmt_Assign (BVar "ProcState_V" (BType_Imm Bit1))
              (BExp_BinPred BIExp_Equal
                 (BExp_BinPred BIExp_SignedLessThan
                    (BExp_BinExp BIExp_Minus
                       (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R2" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 0w)))
                 (BExp_BinPred BIExp_SignedLessOrEqual
                    (BExp_Den (BVar "R2" (BType_Imm Bit64)))
                    (BExp_Den (BVar "R1" (BType_Imm Bit64)))));
            BStmt_Assign (BVar "ProcState_Z" (BType_Imm Bit1))
              (BExp_BinPred BIExp_Equal
                 (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                 (BExp_Den (BVar "R2" (BType_Imm Bit64))))];
         bb_last_statement := BStmt_Jmp (BLE_Label (BL_Label "0x4*"))|>;
       <|bb_label := BL_Address (Imm64 4w);
         bb_statements :=
           [BStmt_Observe 0 (BExp_Const (Imm1 1w)) [BExp_Const (Imm64 4w)] HD];
         bb_last_statement :=
           BStmt_CJmp (BExp_Den (BVar "ProcState_C" (BType_Imm Bit1)))
             (BLE_Label (BL_Address (Imm64 20w)))
             (BLE_Label (BL_Address (Imm64 8w)))|>;
       <|bb_label := BL_Address (Imm64 8w);
         bb_statements :=
           [BStmt_Observe 0 (BExp_Const (Imm1 1w)) [BExp_Const (Imm64 8w)] HD;
            BStmt_Assert
              (BExp_BinPred BIExp_Equal
                 (BExp_BinExp BIExp_And
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R3" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 7w))) (BExp_Const (Imm64 0w)));
            BStmt_Assert
              (BExp_BinExp BIExp_And
                 (BExp_BinPred BIExp_LessOrEqual
                    (BExp_Const (Imm64 0xFFCC0000w))
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R3" (BType_Imm Bit64)))))
                 (BExp_BinPred BIExp_LessThan
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R3" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 0xFFCCAFF0w))));
            BStmt_Observe 0 (BExp_Const (Imm1 1w))
              [BExp_BinExp BIExp_Plus
                 (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                 (BExp_Den (BVar "R3" (BType_Imm Bit64)))] HD;
            BStmt_Assign (BVar "R4" (BType_Imm Bit64))
              (BExp_Load (BExp_Den (BVar "MEM" (BType_Mem Bit64 Bit8)))
                 (BExp_BinExp BIExp_Plus
                    (BExp_Den (BVar "R1" (BType_Imm Bit64)))
                    (BExp_Den (BVar "R3" (BType_Imm Bit64))))
                 BEnd_LittleEndian Bit64)];
         bb_last_statement := BStmt_Jmp (BLE_Label (BL_Address (Imm64 12w)))|>;
       <|bb_label := BL_Address (Imm64 12w);
         bb_statements :=
           [BStmt_Observe 0 (BExp_Const (Imm1 1w)) [BExp_Const (Imm64 12w)]
              HD;
            BStmt_Assign (BVar "R4" (BType_Imm Bit64))
              (BExp_BinExp BIExp_And (BExp_Const (Imm64 0xFFFFFFFFFFFFFFFFw))
                 (BExp_BinExp BIExp_LeftShift
                    (BExp_Den (BVar "R4" (BType_Imm Bit64)))
                    (BExp_Const (Imm64 1w))))];
         bb_last_statement := BStmt_Jmp (BLE_Label (BL_Address (Imm64 16w)))|>;
       <|bb_label := BL_Address (Imm64 16w);
         bb_statements :=
           [BStmt_Observe 0 (BExp_Const (Imm1 1w)) [BExp_Const (Imm64 16w)]
              HD;
            BStmt_Assert
              (BExp_BinPred BIExp_Equal
                 (BExp_BinExp BIExp_And
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R5" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R4" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 7w))) (BExp_Const (Imm64 0w)));
            BStmt_Assert
              (BExp_BinExp BIExp_And
                 (BExp_BinPred BIExp_LessOrEqual
                    (BExp_Const (Imm64 0xFFCC0000w))
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R5" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R4" (BType_Imm Bit64)))))
                 (BExp_BinPred BIExp_LessThan
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R5" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R4" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 0xFFCCAFF0w))));
            BStmt_Observe 0 (BExp_Const (Imm1 1w))
              [BExp_BinExp BIExp_Plus
                 (BExp_Den (BVar "R5" (BType_Imm Bit64)))
                 (BExp_Den (BVar "R4" (BType_Imm Bit64)))] HD;
            BStmt_Assign (BVar "R6" (BType_Imm Bit64))
              (BExp_Load (BExp_Den (BVar "MEM" (BType_Mem Bit64 Bit8)))
                 (BExp_BinExp BIExp_Plus
                    (BExp_Den (BVar "R5" (BType_Imm Bit64)))
                    (BExp_Den (BVar "R4" (BType_Imm Bit64))))
                 BEnd_LittleEndian Bit64)];
         bb_last_statement := BStmt_Jmp (BLE_Label (BL_Address (Imm64 20w)))|>;
       <|bb_label := BL_Address (Imm64 20w);
         bb_statements :=
           [BStmt_Observe 0 (BExp_Const (Imm1 1w)) [BExp_Const (Imm64 20w)]
              HD]; bb_last_statement := BStmt_Halt (BExp_Const (Imm32 0w))|>;
       <|bb_label := BL_Label "0x4*";
         bb_statements :=
           [BStmt_Assert
              (BExp_BinPred BIExp_Equal (BExp_Const (Imm64 41w))
                 (BExp_Const (Imm64 41w)));
            BStmt_Assign (BVar "ProcState_C*" (BType_Imm Bit1))
              (BExp_Den (BVar "ProcState_C" (BType_Imm Bit1)));
            BStmt_Assign (BVar "R1*" (BType_Imm Bit64))
              (BExp_Den (BVar "R1" (BType_Imm Bit64)));
            BStmt_Assign (BVar "R3*" (BType_Imm Bit64))
              (BExp_Den (BVar "R3" (BType_Imm Bit64)));
            BStmt_Assign (BVar "MEM*" (BType_Mem Bit64 Bit8))
              (BExp_Den (BVar "MEM" (BType_Mem Bit64 Bit8)));
            BStmt_Assign (BVar "R4*" (BType_Imm Bit64))
              (BExp_Den (BVar "R4" (BType_Imm Bit64)));
            BStmt_Assign (BVar "R5*" (BType_Imm Bit64))
              (BExp_Den (BVar "R5" (BType_Imm Bit64)))];
         bb_last_statement :=
           BStmt_CJmp
             (BExp_UnaryExp BIExp_Not
                (BExp_Den (BVar "ProcState_C*" (BType_Imm Bit1))))
             (BLE_Label (BL_Label "0x14*")) (BLE_Label (BL_Label "0x8*"))|>;
       <|bb_label := BL_Label "0x8*";
         bb_statements :=
           [BStmt_Assert
              (BExp_BinPred BIExp_Equal
                 (BExp_BinExp BIExp_And
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R1*" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R3*" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 7w))) (BExp_Const (Imm64 0w)));
            BStmt_Assert
              (BExp_BinExp BIExp_And
                 (BExp_BinPred BIExp_LessOrEqual
                    (BExp_Const (Imm64 0xFFCC0000w))
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R1*" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R3*" (BType_Imm Bit64)))))
                 (BExp_BinPred BIExp_LessThan
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R1*" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R3*" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 0xFFCCAFF0w))));
            BStmt_Observe 1 (BExp_Const (Imm1 1w))
              [BExp_BinExp BIExp_Plus
                 (BExp_Den (BVar "R1*" (BType_Imm Bit64)))
                 (BExp_Den (BVar "R3*" (BType_Imm Bit64)))] HD;
            BStmt_Assign (BVar "R4*" (BType_Imm Bit64))
              (BExp_Load (BExp_Den (BVar "MEM*" (BType_Mem Bit64 Bit8)))
                 (BExp_BinExp BIExp_Plus
                    (BExp_Den (BVar "R1*" (BType_Imm Bit64)))
                    (BExp_Den (BVar "R3*" (BType_Imm Bit64))))
                 BEnd_LittleEndian Bit64)];
         bb_last_statement := BStmt_Jmp (BLE_Label (BL_Label "0xC*"))|>;
       <|bb_label := BL_Label "0xC*";
         bb_statements :=
           [BStmt_Assign (BVar "R4*" (BType_Imm Bit64))
              (BExp_BinExp BIExp_And (BExp_Const (Imm64 0xFFFFFFFFFFFFFFFFw))
                 (BExp_BinExp BIExp_LeftShift
                    (BExp_Den (BVar "R4*" (BType_Imm Bit64)))
                    (BExp_Const (Imm64 1w))))];
         bb_last_statement := BStmt_Jmp (BLE_Label (BL_Label "0x10*"))|>;
       <|bb_label := BL_Label "0x10*";
         bb_statements :=
           [BStmt_Assert
              (BExp_BinPred BIExp_Equal
                 (BExp_BinExp BIExp_And
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R5*" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R4*" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 7w))) (BExp_Const (Imm64 0w)));
            BStmt_Assert
              (BExp_BinExp BIExp_And
                 (BExp_BinPred BIExp_LessOrEqual
                    (BExp_Const (Imm64 0xFFCC0000w))
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R5*" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R4*" (BType_Imm Bit64)))))
                 (BExp_BinPred BIExp_LessThan
                    (BExp_BinExp BIExp_Plus
                       (BExp_Den (BVar "R5*" (BType_Imm Bit64)))
                       (BExp_Den (BVar "R4*" (BType_Imm Bit64))))
                    (BExp_Const (Imm64 0xFFCCAFF0w))));
            BStmt_Observe 1 (BExp_Const (Imm1 1w))
              [BExp_BinExp BIExp_Plus
                 (BExp_Den (BVar "R5*" (BType_Imm Bit64)))
                 (BExp_Den (BVar "R4*" (BType_Imm Bit64)))] HD;
            BStmt_Assign (BVar "R6*" (BType_Imm Bit64))
              (BExp_Load (BExp_Den (BVar "MEM*" (BType_Mem Bit64 Bit8)))
                 (BExp_BinExp BIExp_Plus
                    (BExp_Den (BVar "R5*" (BType_Imm Bit64)))
                    (BExp_Den (BVar "R4*" (BType_Imm Bit64))))
                 BEnd_LittleEndian Bit64)];
         bb_last_statement := BStmt_Jmp (BLE_Label (BL_Label "0x14*"))|>;
       <|bb_label := BL_Label "0x14*";
         bb_statements :=
           [BStmt_Assert
              (BExp_BinPred BIExp_Equal (BExp_Const (Imm64 42w))
                 (BExp_Const (Imm64 42w)))];
         bb_last_statement := BStmt_Jmp (BLE_Label (BL_Address (Imm64 4w)))|>]
:bir_val_t bir_program_t
``;

val prog_3_cache_speculation_first = ``
F
``;

val prog_3_test =
  ("prog_3 - spectre_v1", prog_3,
     (prog_3_mem_address_pc,
      ``F``,
      prog_3_cache_tag_index,
      prog_3_cache_tag_only,
      prog_3_cache_index_only,
      prog_3_cache_tag_index_part,
      prog_3_cache_tag_index_part_page,
      prog_3_cache_speculation,
      prog_3_cache_speculation_first)
  );
