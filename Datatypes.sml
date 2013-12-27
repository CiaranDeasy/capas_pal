datatype functor_t = Functor of string;
datatype term_t = Term of functor_t * term_t list;
datatype pred_t = Pred of functor_t * term_t list;
datatype clause_t = Clause of pred_t * pred_t list;
datatype program_t = Program of clause_t list;
datatype query_t = Query of pred_t list;
