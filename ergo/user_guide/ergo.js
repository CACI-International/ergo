var string_merge = {
    className: 'operator',
    begin: /\^/
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
            hljs.COMMENT(/# /, /$/),
            {
                className: 'attribute',
                begin: /## /,
                end: /$/,
                contains: [string_merge]
            },
            {
                className: 'built_in',
                begin: /\b(ergo|pat|fn|index|std|workspace|doc|bind|unset)\b/
            },
            {
                className: 'string',
                begin: '"',
                end: '"',
                contains: [string_merge, backslash_escape]
            },
            {
                className: 'string',
                begin: /' /,
                end: /$/,
                contains: [string_merge]
            },
            {
                className: 'operator',
                begin: /(:|\^|!|->|=)/
            },
            {
                className: 'punctuation',
                begin: /(\(|\)|\{|\}|\[|\]|,|;)/
            },
            {
                className: 'attribute',
                begin: /##/
            },
            {
                className: 'comment',
                begin: /#/
            },
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
