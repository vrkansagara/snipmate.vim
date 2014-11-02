describe 'snippet parser'

    before
        function! Parse(snippet)
            return snipmate#parse#snippet(a:snippet)
        endfunction
        let b:snipmate_visual = 'testvisual'
    end

    it 'parses numeric $id and ${id} vars as [id] lists'
        let expect = [[1234567890]]
        Expect Parse('$1234567890')[0] == expect
        Expect Parse('${1234567890}')[0] == expect
    end

    it 'disregards $ or ${ followed by a non-id'
        Expect Parse('$x1')[0] == ['x1']
        Expect Parse('${x}1')[0] == ['x}1']
        Expect Parse('$VISUA1')[0] == ['VISUA1']
        Expect Parse('${VISUA}1')[0] == ['VISUA}1']
    end

    it 'gathers references to each instance of each stop id'
        let [snip, b:stops] = Parse('x$1x${2:x$1x}x$1x${1/a/b}x$VISUALx')
        function! InstanceFound(list)
            return !empty(filter(copy(b:stops[a:list[0]].instances),
                        \ 'v:val is a:list'))
        endfunction
        function! CheckList(list)
            for item in a:list
                if type(item) == type([])
                    Expect InstanceFound(item) to_be_true
                    call CheckList(item)
                endif
                unlet item " E732
            endfor
        endfunction
        call CheckList(snip)
    end

    it 'parses mirror substitutions ${n/pat/sub} as [n, {...}]'
        let expect = [[1, { 'pat' : 'abc', 'sub' : 'def' }]]
        Expect Parse('${1/abc/def}')[0] == expect
        let expect[0][1].flags = 'g'
        Expect Parse('${1//abc/def}')[0] == expect
    end

    it 'parses vars with placeholders as [id, placeholder] lists'
        Expect Parse('${1:abc}')[0] == [[1, 'abc']]
    end

    it 'evaluates backtick expressions'
        Expect Parse('`fnamemodify("x.y", ":r")`')[0] == ['x']
    end

    it 'parses placeholders for vars and other specials'
        let text = 'a `fnamemodify("x.y", ":r")` ${2:(${3/a/b})}'
        let expect = ['a x ', [2, '(', [3, { 'pat' : 'a', 'sub' : 'b' }], ')']]
        Expect Parse(text)[0] == expect
        Expect Parse(printf('${1:%s}', text))[0] == [[1] + expect]
    end

    it 'converts tabs according to &et, &sts, &sw'
        " &noet -> leave tabs alone
        setl noet
        Expect Parse("abc\tdef\n\t\tghi")[0] == ["abc\tdef", "\t\tghi"]

        " &et -> &sts or &sw
        setl et sts=2 sw=3
        Expect Parse("abc\tdef\n\t\tghi")[0] == ["abc  def", "    ghi"]

        setl et sts=0 sw=3
        Expect Parse("abc\tdef\n\t\tghi")[0] == ["abc   def", "      ghi"]

        setl et sts=-1 sw=3
        Expect Parse("abc\tdef\n\t\tghi")[0] == ["abc   def", "      ghi"]
    end

    it 'parses backslashes as escaping the next character or joining lines'
        Expect Parse('x\x')[0] == ['xx']
        Expect Parse('x\\x')[0] == ['x\x']
        Expect Parse("x\\\nx")[0] == ['xx']
        Expect Parse('x\$1')[0] == ['x$1']
        Expect Parse('${1:\}}')[0] == [[1, '}']]
        Expect Parse('${1/\//\}}')[0] == [[1, { 'pat' : '/', 'sub' : '}' }]]
        Expect Parse('`fnamemodify("\`.x", ":r")`')[0] == ['`']
        Expect Parse('\`x\`')[0] == ['`x`']
    end

    it 'splits text at newlines'
        Expect Parse("x\nx")[0] == ['x', 'x']
    end

    it 'joins evaluated expressions to surrounding text on the same line'
        let g:foo = 'bar'
        Expect Parse("x`g:foo`x")[0] == ['xbarx']
        Expect Parse("x`g:foo`\nx")[0] == ['xbar', 'x']
        Expect Parse("x\n`g:foo`x")[0] == ['x', 'barx']
    end

    it 'adds empty strings before/after vars if at the start/end of a line'
        Expect Parse("x$1\nx")[0] == ['x', [1], '', 'x']
        Expect Parse("x\n$1x")[0] == ['x', '', [1], 'x']
    end

end
