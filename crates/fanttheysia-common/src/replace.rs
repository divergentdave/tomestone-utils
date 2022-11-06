use std::mem::swap;

use tomestone_string_interp::{Expression, MutVisitor, Segment, Text, TextAccumulator};

use crate::StructuredTextRule;

pub struct StructuralFindAndReplace<'a> {
    rules: &'a [StructuredTextRule],
}

impl<'a> StructuralFindAndReplace<'a> {
    pub fn new(rules: &'a [StructuredTextRule]) -> StructuralFindAndReplace {
        StructuralFindAndReplace { rules }
    }

    pub fn visit_text(&mut self, text: &mut Text) {
        let mut old_text = Text::new(Vec::new());
        swap(text, &mut old_text);
        let mut segments = old_text.into_vec();
        self.visit_tag_sequence(&mut segments);
        *text = Text::new(segments);
    }

    pub fn visit_tag_sequence(&mut self, text: &mut Vec<Segment>) {
        for StructuredTextRule { find, replace } in self.rules.iter() {
            if find.is_empty() {
                return;
            }
            // Do a breadth-first search for matching segment subsequences (including
            // partial matches of text literal segments). First, scan the outermost layer.
            let find_leading_literal = match &find[0] {
                Segment::Literal(value) => Some(value),
                _ => None,
            };
            let find_trailing_literal = match &find[find.len() - 1] {
                Segment::Literal(value) => Some(value),
                _ => None,
            };
            if find_leading_literal.is_some() && find_trailing_literal.is_some() && find.len() == 1
            {
                // Special case: the pattern is just a plain substring. Check each
                // literal node, split at any matches, and replace all substrings.
                let find_string = match &find[0] {
                    Segment::Literal(find_string) => find_string,
                    _ => unreachable!(),
                };
                if find_string.is_empty() {
                    return;
                }
                for i in (0..text.len()).rev() {
                    let segment = &text[i];
                    if let Segment::Literal(value) = segment {
                        if !value.contains(find_string) {
                            continue;
                        }
                        let split_parts = value.split(find_string).collect::<Vec<_>>();
                        let mut result = TextAccumulator::with_capacity(
                            replace.len() * (split_parts.len() - 1) + split_parts.len(),
                        );
                        if !split_parts[0].is_empty() {
                            result.push(Segment::Literal(split_parts[0].to_string()));
                        }
                        for split_part in &split_parts[1..] {
                            result.extend(replace.iter().cloned());
                            if !split_part.is_empty() {
                                result.push(Segment::Literal(split_part.to_string()));
                            }
                        }
                        let result_text: Text = result.into();
                        text.splice(i..=i, result_text.into_iter());
                    }
                }
            } else if find.len() == 1 {
                // The pattern is any other single segment. Directly compare segments.
                for i in (0..text.len()).rev() {
                    let segment = &text[i];
                    if segment == &find[0] {
                        text.splice(i..=i, replace.iter().cloned());
                    }
                }
            } else {
                // General case: more than one segment. Both the first and last
                // segments may be text literals, which match based on starts_with/
                // ends_with.
                if find.len() > text.len() {
                    return;
                }
                for i in (0..text.len() + 1 - find.len()).rev() {
                    let mut matches = match (&text[i], &find[0]) {
                        (Segment::Literal(original), Segment::Literal(find_string)) => {
                            original.ends_with(find_string)
                        }
                        (other_text, other_find) => other_text == other_find,
                    };
                    if matches {
                        match (&text[i + find.len() - 1], &find[find.len() - 1]) {
                            (Segment::Literal(original), Segment::Literal(find_string)) => {
                                matches = original.starts_with(find_string);
                            }
                            (other_text, other_find) => matches = other_text == other_find,
                        }
                    }
                    if matches {
                        for (original_segment, find_segment) in text[i + 1..i + find.len() - 1]
                            .iter()
                            .zip(find[1..find.len() - 1].iter())
                        {
                            if original_segment != find_segment {
                                matches = false;
                                break;
                            }
                        }
                    }
                    if matches {
                        let mut result = TextAccumulator::with_capacity(replace.len() + 2);
                        if let (
                            Some(find_leading_literal),
                            Segment::Literal(text_leading_literal),
                        ) = (find_leading_literal, &text[i])
                        {
                            if text_leading_literal.len() > find_leading_literal.len() {
                                result.push(Segment::Literal(
                                    text_leading_literal
                                        [..text_leading_literal.len() - find_leading_literal.len()]
                                        .to_string(),
                                ));
                            }
                        }
                        result.extend(replace.iter().cloned());
                        if let (
                            Some(find_trailing_literal),
                            Segment::Literal(text_trailing_literal),
                        ) = (find_trailing_literal, &text[i + find.len() - 1])
                        {
                            if text_trailing_literal.len() > find_trailing_literal.len() {
                                result.push(Segment::Literal(
                                    text_trailing_literal[find_trailing_literal.len()..]
                                        .to_string(),
                                ));
                            }
                        }
                        text.splice(i..i + find.len(), Text::from(result).into_iter());
                    }
                }
            }
        }

        for segment in text {
            self.visit_tag(segment);
        }
    }
}

impl<'a> MutVisitor for StructuralFindAndReplace<'a> {
    fn visit_tag(&mut self, tag: &mut Segment) {
        self.recurse_tag(tag);
    }

    fn visit_expression(&mut self, expr: &mut Expression) {
        if let Expression::Text(text) = expr {
            self.visit_text(text);
        } else {
            self.recurse_expression(expr);
        }
    }
}

#[cfg(test)]
mod tests {
    use tomestone_string_interp::{Expression, Segment, Text};

    use crate::{StructuralFindAndReplace, StructuredTextRule};

    #[test]
    fn replace_substring_with_text() {
        let rules = [StructuredTextRule {
            find: vec![Segment::Literal("needle".to_string())],
            replace: vec![Segment::Literal("new text".to_string())],
        }];

        let mut visitor = StructuralFindAndReplace::new(&rules);

        let mut text = vec![];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![]);

        let mut text = vec![Segment::Literal("needle".to_string())];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![Segment::Literal("new text".to_string())]);

        let mut text = vec![Segment::Literal("needleneedle".to_string())];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![Segment::Literal("new textnew text".to_string())]);

        let mut text = vec![Segment::Literal("needle needle".to_string())];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![Segment::Literal("new text new text".to_string())]
        );

        let mut text = vec![Segment::Literal(" needle needle ".to_string())];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![Segment::Literal(" new text new text ".to_string())]
        );

        let mut text = vec![
            Segment::Literal("first needle".to_string()),
            Segment::NewLine,
            Segment::Literal("needle number two".to_string()),
        ];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![
                Segment::Literal("first new text".to_string()),
                Segment::NewLine,
                Segment::Literal("new text number two".to_string()),
            ]
        );
    }

    #[test]
    fn replace_substring_with_nodes() {
        let rules = [StructuredTextRule {
            find: vec![Segment::Literal("needle".to_string())],
            replace: vec![Segment::NewLine, Segment::Dash, Segment::NewLine],
        }];

        let mut visitor = StructuralFindAndReplace::new(&rules);

        let mut text = vec![];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![]);

        let mut text = vec![Segment::Literal("needle".to_string())];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![Segment::NewLine, Segment::Dash, Segment::NewLine]
        );

        let mut text = vec![Segment::Literal("needleneedle".to_string())];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine,
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine,
            ]
        );

        let mut text = vec![Segment::Literal("needle needle".to_string())];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine,
                Segment::Literal(" ".to_string()),
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine,
            ]
        );

        let mut text = vec![Segment::Literal(" needle needle ".to_string())];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![
                Segment::Literal(" ".to_string()),
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine,
                Segment::Literal(" ".to_string()),
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine,
                Segment::Literal(" ".to_string()),
            ]
        );

        let mut text = vec![
            Segment::Literal("first needle".to_string()),
            Segment::NewLine,
            Segment::Literal("needle number two".to_string()),
        ];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![
                Segment::Literal("first ".to_string()),
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine,
                Segment::NewLine,
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine,
                Segment::Literal(" number two".to_string()),
            ]
        );

        let mut text = vec![Segment::StringValue(Expression::Text(Text::new(vec![
            Segment::Literal("needle".to_string()),
        ])))];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(
            text,
            vec![Segment::StringValue(Expression::Text(Text::new(vec![
                Segment::NewLine,
                Segment::Dash,
                Segment::NewLine
            ])))]
        );
    }

    #[test]
    fn replace_start_and_end_with_literals() {
        let rules = [StructuredTextRule {
            find: vec![
                Segment::Literal("left".to_string()),
                Segment::Dash,
                Segment::Literal("right".to_string()),
            ],
            replace: vec![Segment::Literal("new".to_string())],
        }];

        let mut visitor = StructuralFindAndReplace::new(&rules);

        let mut text = vec![];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![]);

        let mut text = vec![
            Segment::Literal("left".to_string()),
            Segment::Dash,
            Segment::Literal("right".to_string()),
        ];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![Segment::Literal("new".to_string())]);

        let mut text = vec![
            Segment::Literal(" left".to_string()),
            Segment::Dash,
            Segment::Literal("right".to_string()),
        ];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![Segment::Literal(" new".to_string())]);

        let mut text = vec![
            Segment::Literal("left".to_string()),
            Segment::Dash,
            Segment::Literal("right ".to_string()),
        ];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![Segment::Literal("new ".to_string())]);

        let mut text = vec![
            Segment::Literal("left".to_string()),
            Segment::Dash,
            Segment::Literal("rightleft".to_string()),
            Segment::Dash,
            Segment::Literal("right".to_string()),
        ];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![Segment::Literal("newnew".to_string())]);

        let mut text = vec![
            Segment::Literal("left".to_string()),
            Segment::Dash,
            Segment::Literal("right left".to_string()),
            Segment::Dash,
            Segment::Literal("right".to_string()),
        ];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![Segment::Literal("new new".to_string())]);

        let mut text = vec![
            Segment::Literal(" left".to_string()),
            Segment::Dash,
            Segment::Literal("right left".to_string()),
            Segment::Dash,
            Segment::Literal("right ".to_string()),
        ];
        visitor.visit_tag_sequence(&mut text);
        assert_eq!(text, vec![Segment::Literal(" new new ".to_string())]);
    }
}
