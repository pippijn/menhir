(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in file LICENSE.                                     *)
(*                                                                        *)
(**************************************************************************)

type grammar = 
    {
      p_preludes	   : Stretch.t list;
      p_postludes          : Syntax.trailer list;
      p_parameters         : Stretch.t list;
      p_start_symbols      : Positions.t StringMap.t;
      p_types              : Stretch.ocamltype Positions.located StringMap.t;
      p_tokens	           : Syntax.token_properties StringMap.t;
      p_rules	           : Syntax.parameterized_rule StringMap.t;
    }
    
