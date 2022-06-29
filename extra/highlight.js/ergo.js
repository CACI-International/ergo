var string_interp = {
    className: 'operator',
    begin: /\$/
};

var backslash_escape = {
    className: 'char.escape',
    begin: /\\(n|t|\\|"|u\{[0-9a-fA-F]{1-6}\})/,
    relevance: 0
};

/*
Language: Ergo
*/
function ergo_lang(hljs) {
    return {
        name: "Ergo",
        contains: [
            // Line comments
            hljs.COMMENT(/# /, /$/),
            // Doc comments
            {
                className: 'attribute',
                begin: /## /,
                end: /$/,
                contains: [string_interp]
            },
            // Built-in functions
            {
                className: 'built_in',
                begin: /(\b(ergo|fn|index|std|workspace|doc|late-bind|bind|unset)\b|!id\b|!no-id\b)/
            },
            // Quoted strings
            {
                className: 'string',
                begin: '"',
                end: '"',
                contains: [string_interp, backslash_escape]
            },
            // Block strings
            {
                className: 'string',
                begin: /' /,
                end: /$/,
                contains: [string_interp]
            },
            // Operators
            {
                className: 'operator',
                begin: /(:|\$\??|\^|->|=|~)/
            },
            // Grouping
            {
                className: 'punctuation',
                begin: /(\(|\)|\{|\}|\[|\]|,|;)/
            },
            // Attributes
            {
                className: 'attribute',
                begin: /##/
            },
            // Tree comments
            {
                className: 'comment',
                begin: /#/
            },
            // Pipe operators
            {
                className: 'meta',
                begin: /(\|>?|<\|)/
            }
        ]
    };
}

if (window.hljs) {
    window.hljs.registerLanguage("ergo", ergo_lang);
    window.hljs.initHighlighting();
}
