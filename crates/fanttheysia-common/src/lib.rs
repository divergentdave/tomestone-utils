use serde::{Deserialize, Serialize};

use tomestone_string_interp::{Expression, Segment, TreeNode, Visitor};

/// Visitor to determine whether an expression tree uses the gender player parameter.
///
/// # Example
/// ```
/// use fanttheysia_common::GenderExpressionVisitor;
/// use tomestone_string_interp::{Expression, TreeNode};
///
/// let expression = Expression::Equal(Box::new((
///     Expression::PlayerParameter(4),
///     Expression::Integer(1),
/// )));
/// let mut visitor = GenderExpressionVisitor::new();
/// expression.accept(&mut visitor);
/// assert!(visitor.flag);
/// ```
pub struct GenderExpressionVisitor {
    /// After walking an expression, this flag will be set if any sub-expression read from the
    /// gender player parameter.
    pub flag: bool,
}

impl GenderExpressionVisitor {
    /// Construct a new visitor.
    pub fn new() -> GenderExpressionVisitor {
        GenderExpressionVisitor { flag: false }
    }
}

impl Default for GenderExpressionVisitor {
    fn default() -> GenderExpressionVisitor {
        GenderExpressionVisitor::new()
    }
}

impl Visitor for GenderExpressionVisitor {
    fn visit_expression(&mut self, expr: &Expression) {
        if let Expression::PlayerParameter(parameter_index) = expr {
            if *parameter_index == 4 {
                self.flag = true;
                return;
            }
        }
        self.recurse_expression(expr);
    }
}

/// This struct is a copy of the `Expression::IfSegment` variant, so they can be collected,
/// compared, and transformed.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct IfSegment {
    pub condition: Expression,
    pub true_value: Expression,
    pub false_value: Expression,
}

/// This visitor clones and collects all if-else text segments where the if-else condition depends
/// on the gender player parameter.
///
/// # Example
/// ```
/// use fanttheysia_common::GenderConditionalTextVisitor;
/// use tomestone_string_interp::{Expression, Segment, Text, TreeNode};
///
/// let text = Text::new(vec![Segment::Literal("Good morning, ".to_string()), Segment::If {
///     condition: Expression::PlayerParameter(4),
///     true_value: Expression::Text(Text::new(vec![Segment::Literal("madam".to_string())])),
///     false_value: Expression::Text(Text::new(vec![Segment::Literal("sir".to_string())])),
/// }]);
/// let mut visitor = GenderConditionalTextVisitor::new();
/// text.accept(&mut visitor);
/// assert_eq!(visitor.ifs.len(), 1);
/// println!("{:?}", visitor.ifs);
pub struct GenderConditionalTextVisitor {
    pub ifs: Vec<IfSegment>,
}

impl GenderConditionalTextVisitor {
    /// Construct a new visitor.
    pub fn new() -> GenderConditionalTextVisitor {
        GenderConditionalTextVisitor { ifs: Vec::new() }
    }
}

impl Default for GenderConditionalTextVisitor {
    fn default() -> GenderConditionalTextVisitor {
        GenderConditionalTextVisitor::new()
    }
}

impl Visitor for GenderConditionalTextVisitor {
    fn visit_tag(&mut self, tag: &Segment) {
        if let Segment::If {
            condition,
            true_value,
            false_value,
        } = tag
        {
            let mut expression_visitor = GenderExpressionVisitor::new();
            condition.accept(&mut expression_visitor);
            if expression_visitor.flag {
                self.ifs.push(IfSegment {
                    condition: condition.clone(),
                    true_value: true_value.clone(),
                    false_value: false_value.clone(),
                });
            } else {
                self.recurse_tag(tag);
            }
        } else {
            self.recurse_tag(tag);
        }
    }

    fn visit_expression(&mut self, expr: &Expression) {
        self.recurse_expression(expr);
    }
}

// TODO: management of backing up/restoring files from sqpack directory
