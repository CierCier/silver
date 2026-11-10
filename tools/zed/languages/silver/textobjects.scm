(function_definition
  body: (block
    "{"
    _* @function.inside
    "}")) @function.around

(macro_definition
  body: (block
    "{"
    _* @function.inside
    "}")) @function.around

(struct_definition
  body: (field_list
    "{"
    _* @class.inside
    "}")) @class.around

(enum_definition
  body: (variant_list
    "{"
    _* @class.inside
    "}")) @class.around

(trait_definition
  body: (member_list
    "{"
    _* @class.inside
    "}")) @class.around

(impl_block
  body: (member_list
    "{"
    _* @class.inside
    "}")) @class.around

(line_comment)+ @comment.around

(block_comment) @comment.around
