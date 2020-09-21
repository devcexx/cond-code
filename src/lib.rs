#![feature(external_doc)]
#![doc(include = "../README.md")]
#![feature(extend_one)]

mod error;
mod util;

use error::Result;
use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::quote;
use util::{require_eof, require_group, require_punct, require_token};

// FIXME Current span reporting is a mess. Fix it.

const COMMA: char = ',';
const AT_SYMBOL: char = '@';

#[derive(Debug)]
struct TemplateGroup {
    delimiter: Delimiter,
    tokens: Vec<TemplateNode>
}

impl TemplateGroup {
    fn new(delimiter: Delimiter, tokens: Vec<TemplateNode>) -> Self {
        Self { delimiter, tokens }
    }
}

#[derive(Debug)]
struct TemplateCondBlock {
    cond_name: String,
    tokens: TokenStream
}

impl TemplateCondBlock {
    fn new(cond_name: String, tokens: TokenStream) -> Self {
        Self { cond_name, tokens }
    }
}

/// Represents a element of the code of a template. Each element might be, either a raw token that
/// shouldn't be processed; or a block of tokens that is expanded or not depending on a condition.
#[derive(Debug)]
enum TemplateNode {
    RawToken(TokenTree),
    Group(TemplateGroup),
    CondBlock(TemplateCondBlock)
}

/// Encapsulates information of a code_template! macro.
#[derive(Debug)]
struct CodeTemplate {
    /// The name of the field.
    name: String,

    /// The parameters that defines the template.
    parameters: Vec<String>,

    /// The code that defines the template, as a vector of `TemplateElem`.
    code: Vec<TemplateNode>
}

impl CodeTemplate {
    fn new(name: String, parameters: Vec<String>, code: Vec<TemplateNode>) -> Self {
        Self {
            name,
            parameters,
            code
        }
    }

    /// Parses a parameter list defined inside a group by a arbitrary number of IDENT tokens
    /// separated each one with a comma PUNCT token.
    fn parse_parameters(input: Group) -> Result<Vec<String>> {
        if input.delimiter() != Delimiter::Parenthesis {
            return Err(("Expected parenthesis", input.span()).into());
        }

        let mut params = Vec::new();
        let mut expect_comma = false;

        for token in input.stream().into_iter() {
            if expect_comma {
                match token {
                    TokenTree::Punct(token_comma) if token_comma.as_char() == COMMA => {
                        expect_comma = false;
                    }
                    _ => return Err(("Expected comma", token.span()).into())
                }
            } else {
                if let TokenTree::Ident(token_ident) = token {
                    params.push(token_ident.to_string());
                    expect_comma = true;
                } else {
                    return Err(("Expected identifier", token.span()).into());
                }
            }
        }

        Ok(params)
    }

    /// Probes if a group is a conditional block and, if so, parses it.
    ///
    /// If the given group seems to be a conditional block (starts with `[@`...`]`), then it is
    /// parsed and returned as a [`TemplateCondBlock`]. Otherwise, `None` is returned instead. If
    /// the group seems has the syntax of a conditional block, but it fails to be parsed, then an
    /// error is returned.
    ///
    /// [`TemplateCondBlock`]: #struct.TemplateCondBlock
    fn parse_template_cond_block(
        group: &Group,
        av_params: &Vec<String>
    ) -> Result<Option<TemplateCondBlock>> {
        if group.delimiter() == Delimiter::Bracket {
            let tokens = group.stream();
            let ref mut it = tokens.into_iter();

            if let Ok(_) = require_punct(it, AT_SYMBOL) {
                let cond_token: Ident = require_token(it)?;
                let cond_name = cond_token.to_string();

                if av_params.contains(&cond_name) {
                    let mut final_stream = TokenStream::new();
                    final_stream.extend(it);
                    Ok(Some(TemplateCondBlock::new(cond_name, final_stream)))
                } else {
                    Err((
                        format!("Invalid conditional value {}", cond_name),
                        cond_token.span()
                    )
                        .into())
                }
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    /// Parses the block code of a template categorizing each token as either a raw token or a
    /// template block, containing conditional code.
    fn parse_template_block(
        it: &mut dyn Iterator<Item = TokenTree>,
        av_params: &Vec<String>
    ) -> Result<Vec<TemplateNode>> {
        let mut elems = Vec::new();

        for token in it {
            elems.push(match token {
                TokenTree::Group(group) => {
                    if let Some(template_block) =
                        CodeTemplate::parse_template_cond_block(&group, av_params)?
                    {
                        TemplateNode::CondBlock(template_block)
                    } else {
                        let inner_group = CodeTemplate::parse_template_block(
                            &mut group.stream().into_iter(),
                            &av_params
                        )?;
                        TemplateNode::Group(TemplateGroup::new(group.delimiter(), inner_group))
                    }
                }
                token => TemplateNode::RawToken(token)
            })
        }

        Ok(elems)
    }

    /// Parses the full body of a code_template! macro into a [`CodeTemplate`] object.
    /// [`CodeTemplate`]: #struct.CodeTemplate
    fn parse(input: TokenStream) -> Result<CodeTemplate> {
        let ref mut it = input.into_iter();

        let name = require_token::<Ident>(it)?;
        require_punct(it, COMMA)?;
        let parameters =
            CodeTemplate::parse_parameters(require_group(it, Delimiter::Parenthesis)?)?;
        require_punct(it, COMMA)?;
        let block = require_group(it, Delimiter::Brace)?;
        require_eof(it)?;

        let code =
            CodeTemplate::parse_template_block(&mut block.stream().into_iter(), &parameters)?;

        Ok(CodeTemplate::new(name.to_string(), parameters, code))
    }

    /// Expands the code of a template cosidering active only the given parameters.
    fn expand_tree(tree: &Vec<TemplateNode>, active_params: &Vec<&String>) -> TokenStream {
        let mut stream = TokenStream::new();

        for elem in tree {
            match elem {
                TemplateNode::RawToken(token) => stream.extend_one(token.clone()),

                TemplateNode::Group(token) => stream.extend_one(TokenTree::Group(Group::new(
                    token.delimiter,
                    CodeTemplate::expand_tree(&token.tokens, &active_params)
                ))),

                TemplateNode::CondBlock(block) => {
                    if active_params.contains(&&block.cond_name) {
                        stream.extend(block.tokens.clone());
                    }
                }
            }
        }

        stream
    }

    /// Expands the code of a template cosidering active only the given parameters.
    fn expand(&self, active_params: &Vec<&String>) -> TokenStream {
        return CodeTemplate::expand_tree(&self.code, active_params);
    }
}

/// Macro that allows to define a code template.
///
/// This macro takes a name, a list of parameters names and a block of code with conditional blocks,
/// and generates a declarative macro that allows to render the block of code in any other place,
/// replacing the conditional blocks depending on the passed parameters. The syntax of this macros
/// is defined as follows (using macro_rules-like syntax):
///
/// ```ignore
/// code_template!(
///     $name:ident,
///     ($($param:ident),*),
///     $code:block
/// );
/// ```
///
/// # Arguments
///
/// There code_template! macro defines three arguments:
///  * `name`: The name that will identify the template, and will be used to generate the name of
///  the declarative macro that must be called in order to render this template. The final name of
///  this macro would be: `code_template_gen_$name`
///
/// * `$($param:ident),*`: a comma-delimited list of parameters that specifies in what parameters
/// the expansion of this templates depends on. You may specify one of this parameters on the
/// conditional blocks of this template for selecting whether or not these blocks should be rendered
/// or omitted.
///
/// * `$code`: A block of code, delimited by `{...}` that defines the code of this template. It may
/// contain conditional blocks.
///
/// # Conditional blocks
///
/// You may use conditional blocks inside of a template for conditionally generate code depending on
/// the value of the parameters of the template. A conditional block is defined as follows:
///
/// ```text
/// [@<param> <code>]
/// ```
///
/// The conditional block defined above stands that the code `<code>` will only be present after the
/// expansion of the macro when parameter `<param>` is set to `true`. Otherwise, it won't have any
/// effect on the final code.
///
/// # Example
///
/// ```
/// use cond_code::code_template;
///
/// code_template!(test, (with_a, with_b), {
///     // Prevent warnings when a conditional block is expanded to nothing.
///     #[allow(redundant_semicolons)]
///     pub fn test() -> String {
///         let mut string = "".to_owned();
///         [@with_a string += "A"; ];
///         [@with_b string += "B"; ];
///         string += "C";
///
///         string
///     }
/// });
///
/// mod test_none {
///     code_template_gen_test!(with_a: false, with_b: false);
/// }
/// mod test_a {
///     code_template_gen_test!(with_a: true, with_b: false);
/// }
/// mod test_b {
///     code_template_gen_test!(with_a: false, with_b: true);
/// }
/// mod test_ab {
///     code_template_gen_test!(with_a: true, with_b: true);
/// }

/// assert_eq!("C", test_none::test());
/// assert_eq!("AC", test_a::test());
/// assert_eq!("BC", test_b::test());
/// assert_eq!("ABC", test_ab::test());
/// ```
/// 
/// # Limitations
///
/// This macro has the following limitations:
///
/// * This macro does not perform anything else but replacing conditional blocks. Don't expect any
/// other functionality like magical `ident`s concatenation.

///
/// * Each conditional block must have only one condition associated. No multiple conditions /
/// complex expressions are allowed for determining if a block shoud be rendered or not.

///
/// * When calling generation macro (`code_template_gen_*!`, see the example above), all the defined
/// parameters in the `code_template!` macro must be specified, and in the same order.

///
/// * For generating the expanded code, this macro, for know, permutates all the possible values of
/// the defined parameters, and generates one declarative macro rule for each one. This means that
/// the number of rules is 2<sup>n</sup>, where n is the number of defined parameters. Keep this
/// number low.
#[proc_macro]
pub fn code_template(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let template = CodeTemplate::parse(item.into());

    match template {
        Ok(template) => gen_macro_for_code_template(&template).into(),
        Err(err) => err.to_compile_error().into()
    }
}

fn gen_macro_for_code_template(template: &CodeTemplate) -> TokenStream {
    let macro_name = Ident::new(
        &format!("code_template_gen_{}", template.name),
        Span::call_site()
    );

    let ref params = template.parameters;
    let npermutations = 1 << params.len();
    let mut macro_branches: TokenStream = TokenStream::new();

    fn is_param_active(npermut: i32, index: usize) -> bool {
        (npermut & (1 << index)) > 0
    }

    for i in 0..npermutations {
        let mut pattern = TokenStream::new();

        for param_index in 0..params.len() {
            if param_index > 0 {
                // Add comma
                pattern.extend_one(TokenTree::Punct(Punct::new(COMMA, Spacing::Alone)));
            }

            // Add parameter
            pattern.extend_one(TokenTree::Ident(Ident::new(
                &params[param_index],
                Span::call_site()
            )));
            pattern.extend_one(TokenTree::Punct(Punct::new(':', Spacing::Alone)));

            let param_state_token;
            if is_param_active(i, param_index) {
                param_state_token = Ident::new("true", Span::call_site());
            } else {
                param_state_token = Ident::new("false", Span::call_site());
            }
            pattern.extend_one(TokenTree::Ident(param_state_token));
        }

        let active_params = (0..params.len())
            .filter(|&index| is_param_active(i, index))
            .map(|index| &params[index])
            .collect::<Vec<&String>>();

        let expansion = template.expand(&active_params);

        #[rustfmt::skip]
        macro_branches.extend(quote! {
            (#pattern) => {
		#expansion
            };
        });
    }

    #[rustfmt::skip]
    quote! {
	macro_rules! #macro_name {
            #macro_branches
	}
    }
}
