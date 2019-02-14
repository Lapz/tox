Program {
    functions: [
        Spanned {
            span: Span {
                start: Position {
                    line: 6,
                    column: 1,
                    absolute: 43
                },
                end: Position {
                    line: 8,
                    column: 1,
                    absolute: 120
                }
            },
            value: Function {
                name: Spanned {
                    span: Span {
                        start: Position {
                            line: 6,
                            column: 4,
                            absolute: 46
                        },
                        end: Position {
                            line: 6,
                            column: 8,
                            absolute: 50
                        }
                    },
                    value: ItemName {
                        name: Spanned {
                            span: Span {
                                start: Position {
                                    line: 6,
                                    column: 4,
                                    absolute: 46
                                },
                                end: Position {
                                    line: 6,
                                    column: 8,
                                    absolute: 50
                                }
                            },
                            value: Symbol(
                                9
                            )
                        },
                        type_params: []
                    }
                },
                params: Spanned {
                    span: Span {
                        start: Position {
                            line: 6,
                            column: 8,
                            absolute: 50
                        },
                        end: Position {
                            line: 6,
                            column: 9,
                            absolute: 51
                        }
                    },
                    value: []
                },
                body: Spanned {
                    span: Span {
                        start: Position {
                            line: 6,
                            column: 11,
                            absolute: 53
                        },
                        end: Position {
                            line: 8,
                            column: 1,
                            absolute: 120
                        }
                    },
                    value: Block(
                        [
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 7,
                                        column: 5,
                                        absolute: 59
                                    },
                                    end: Position {
                                        line: 7,
                                        column: 64,
                                        absolute: 118
                                    }
                                },
                                value: VarDeclaration {
                                    ident: Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 7,
                                                column: 9,
                                                absolute: 63
                                            },
                                            end: Position {
                                                line: 7,
                                                column: 10,
                                                absolute: 64
                                            }
                                        },
                                        value: Symbol(
                                            10
                                        )
                                    },
                                    ty: None,
                                    expr: Some(
                                        Spanned {
                                            span: Span {
                                                start: Position {
                                                    line: 7,
                                                    column: 13,
                                                    absolute: 67
                                                },
                                                end: Position {
                                                    line: 7,
                                                    column: 63,
                                                    absolute: 117
                                                }
                                            },
                                            value: Constructor {
                                                enum_name: Spanned {
                                                    span: Span {
                                                        start: Position {
                                                            line: 7,
                                                            column: 13,
                                                            absolute: 67
                                                        },
                                                        end: Position {
                                                            line: 7,
                                                            column: 17,
                                                            absolute: 71
                                                        }
                                                    },
                                                    value: Symbol(
                                                        5
                                                    )
                                                },
                                                variant: Spanned {
                                                    span: Span {
                                                        start: Position {
                                                            line: 7,
                                                            column: 19,
                                                            absolute: 73
                                                        },
                                                        end: Position {
                                                            line: 7,
                                                            column: 23,
                                                            absolute: 77
                                                        }
                                                    },
                                                    value: Symbol(
                                                        6
                                                    )
                                                },
                                                args: [
                                                    Spanned {
                                                        span: Span {
                                                            start: Position {
                                                                line: 7,
                                                                column: 24,
                                                                absolute: 78
                                                            },
                                                            end: Position {
                                                                line: 7,
                                                                column: 25,
                                                                absolute: 79
                                                            }
                                                        },
                                                        value: Literal(
                                                            Int(
                                                                1
                                                            )
                                                        )
                                                    },
                                                    Spanned {
                                                        span: Span {
                                                            start: Position {
                                                                line: 7,
                                                                column: 26,
                                                                absolute: 80
                                                            },
                                                            end: Position {
                                                                line: 7,
                                                                column: 62,
                                                                absolute: 116
                                                            }
                                                        },
                                                        value: Constructor {
                                                            enum_name: Spanned {
                                                                span: Span {
                                                                    start: Position {
                                                                        line: 7,
                                                                        column: 26,
                                                                        absolute: 80
                                                                    },
                                                                    end: Position {
                                                                        line: 7,
                                                                        column: 30,
                                                                        absolute: 84
                                                                    }
                                                                },
                                                                value: Symbol(
                                                                    5
                                                                )
                                                            },
                                                            variant: Spanned {
                                                                span: Span {
                                                                    start: Position {
                                                                        line: 7,
                                                                        column: 32,
                                                                        absolute: 86
                                                                    },
                                                                    end: Position {
                                                                        line: 7,
                                                                        column: 36,
                                                                        absolute: 90
                                                                    }
                                                                },
                                                                value: Symbol(
                                                                    6
                                                                )
                                                            },
                                                            args: [
                                                                Spanned {
                                                                    span: Span {
                                                                        start: Position {
                                                                            line: 7,
                                                                            column: 37,
                                                                            absolute: 91
                                                                        },
                                                                        end: Position {
                                                                            line: 7,
                                                                            column: 38,
                                                                            absolute: 92
                                                                        }
                                                                    },
                                                                    value: Literal(
                                                                        Int(
                                                                            2
                                                                        )
                                                                    )
                                                                },
                                                                Spanned {
                                                                    span: Span {
                                                                        start: Position {
                                                                            line: 7,
                                                                            column: 39,
                                                                            absolute: 93
                                                                        },
                                                                        end: Position {
                                                                            line: 7,
                                                                            column: 61,
                                                                            absolute: 115
                                                                        }
                                                                    },
                                                                    value: Constructor {
                                                                        enum_name: Spanned {
                                                                            span: Span {
                                                                                start: Position {
                                                                                    line: 7,
                                                                                    column: 39,
                                                                                    absolute: 93
                                                                                },
                                                                                end: Position {
                                                                                    line: 7,
                                                                                    column: 43,
                                                                                    absolute: 97
                                                                                }
                                                                            },
                                                                            value: Symbol(
                                                                                11
                                                                            )
                                                                        },
                                                                        variant: Spanned {
                                                                            span: Span {
                                                                                start: Position {
                                                                                    line: 7,
                                                                                    column: 45,
                                                                                    absolute: 99
                                                                                },
                                                                                end: Position {
                                                                                    line: 7,
                                                                                    column: 49,
                                                                                    absolute: 103
                                                                                }
                                                                            },
                                                                            value: Symbol(
                                                                                6
                                                                            )
                                                                        },
                                                                        args: [
                                                                            Spanned {
                                                                                span: Span {
                                                                                    start: Position {
                                                                                        line: 7,
                                                                                        column: 50,
                                                                                        absolute: 104
                                                                                    },
                                                                                    end: Position {
                                                                                        line: 7,
                                                                                        column: 51,
                                                                                        absolute: 105
                                                                                    }
                                                                                },
                                                                                value: Literal(
                                                                                    Int(
                                                                                        3
                                                                                    )
                                                                                )
                                                                            },
                                                                            Spanned {
                                                                                span: Span {
                                                                                    start: Position {
                                                                                        line: 7,
                                                                                        column: 52,
                                                                                        absolute: 106
                                                                                    },
                                                                                    end: Position {
                                                                                        line: 7,
                                                                                        column: 56,
                                                                                        absolute: 110
                                                                                    }
                                                                                },
                                                                                value: Constructor {
                                                                                    enum_name: Spanned {
                                                                                        span: Span {
                                                                                            start: Position {
                                                                                                line: 7,
                                                                                                column: 52,
                                                                                                absolute: 106
                                                                                            },
                                                                                            end: Position {
                                                                                                line: 7,
                                                                                                column: 56,
                                                                                                absolute: 110
                                                                                            }
                                                                                        },
                                                                                        value: Symbol(
                                                                                            5
                                                                                        )
                                                                                    },
                                                                                    variant: Spanned {
                                                                                        span: Span {
                                                                                            start: Position {
                                                                                                line: 7,
                                                                                                column: 58,
                                                                                                absolute: 112
                                                                                            },
                                                                                            end: Position {
                                                                                                line: 7,
                                                                                                column: 61,
                                                                                                absolute: 115
                                                                                            }
                                                                                        },
                                                                                        value: Symbol(
                                                                                            8
                                                                                        )
                                                                                    },
                                                                                    args: []
                                                                                }
                                                                            }
                                                                        ]
                                                                    }
                                                                }
                                                            ]
                                                        }
                                                    }
                                                ]
                                            }
                                        }
                                    )
                                }
                            }
                        ]
                    )
                },
                returns: None
            }
        }
    ],
    classes: [],
    aliases: [],
    enums: [
        Spanned {
            span: Span {
                start: Position {
                    line: 1,
                    column: 1,
                    absolute: 0
                },
                end: Position {
                    line: 4,
                    column: 1,
                    absolute: 40
                }
            },
            value: Enum {
                name: Spanned {
                    span: Span {
                        start: Position {
                            line: 1,
                            column: 6,
                            absolute: 5
                        },
                        end: Position {
                            line: 1,
                            column: 10,
                            absolute: 9
                        }
                    },
                    value: ItemName {
                        name: Spanned {
                            span: Span {
                                start: Position {
                                    line: 1,
                                    column: 6,
                                    absolute: 5
                                },
                                end: Position {
                                    line: 1,
                                    column: 10,
                                    absolute: 9
                                }
                            },
                            value: Symbol(
                                5
                            )
                        },
                        type_params: []
                    }
                },
                variants: [
                    EnumVariant {
                        name: Spanned {
                            span: Span {
                                start: Position {
                                    line: 2,
                                    column: 5,
                                    absolute: 16
                                },
                                end: Position {
                                    line: 2,
                                    column: 9,
                                    absolute: 20
                                }
                            },
                            value: Symbol(
                                6
                            )
                        },
                        inner: [
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 2,
                                        column: 10,
                                        absolute: 21
                                    },
                                    end: Position {
                                        line: 2,
                                        column: 13,
                                        absolute: 24
                                    }
                                },
                                value: Simple(
                                    Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 2,
                                                column: 10,
                                                absolute: 21
                                            },
                                            end: Position {
                                                line: 2,
                                                column: 13,
                                                absolute: 24
                                            }
                                        },
                                        value: Symbol(
                                            7
                                        )
                                    }
                                )
                            },
                            Spanned {
                                span: Span {
                                    start: Position {
                                        line: 2,
                                        column: 14,
                                        absolute: 25
                                    },
                                    end: Position {
                                        line: 2,
                                        column: 18,
                                        absolute: 29
                                    }
                                },
                                value: Simple(
                                    Spanned {
                                        span: Span {
                                            start: Position {
                                                line: 2,
                                                column: 14,
                                                absolute: 25
                                            },
                                            end: Position {
                                                line: 2,
                                                column: 18,
                                                absolute: 29
                                            }
                                        },
                                        value: Symbol(
                                            5
                                        )
                                    }
                                )
                            }
                        ]
                    },
                    EnumVariant {
                        name: Spanned {
                            span: Span {
                                start: Position {
                                    line: 3,
                                    column: 5,
                                    absolute: 36
                                },
                                end: Position {
                                    line: 3,
                                    column: 8,
                                    absolute: 39
                                }
                            },
                            value: Symbol(
                                8
                            )
                        },
                        inner: []
                    }
                ]
            }
        }
    ]
}
