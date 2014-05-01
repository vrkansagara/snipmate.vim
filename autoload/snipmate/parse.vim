" Snippet definition parsing code

" The next two functions were essentially taken from Fugitive
" <https://github.com/tpope/vim-fugitive>
function! s:function(name) abort
    return function(matchstr(expand('<sfile>'), '<SNR>\d\+_') . a:name)
endfunction

function! s:add_methods(namespace, method_names) abort
    for name in a:method_names
        let s:{a:namespace}_proto[name] = s:function(a:namespace . '_' . name)
    endfor
endfunction

let s:parser_proto = {}

function! s:new_parser(text)
    let ret = copy(s:parser_proto)
    let ret.input = a:text
    let ret.len = strlen(ret.input)
    let ret.pos = 0
    let ret.next = ret.input[ret.pos]
    let ret.vars = {}
    return ret
endfunction

function! s:parser_advance(...) dict
    let self.pos += a:0 ? a:1 : 1
    let self.next = self.input[self.pos]
endfunction

function! s:parser_same(tok) dict
    if self.next == a:tok
        call self.advance()
        return 1
    else
        return 0
    endif
endfunction

function! s:parser_id() dict
    if self.input[(self.pos):(self.pos+5)] == 'VISUAL'
        call self.advance(6)
        return 'VISUAL'
    elseif self.next =~ '\d'
        let end = matchend(self.input, '\d\+', self.pos)
        let res = strpart(self.input, self.pos, end - self.pos)
        call self.advance(end - self.pos)
        return +res " force conversion to Number
    endif
    return -1
endfunction

function! s:parser_add_var(var) dict
    let id = a:var[0]
    if !has_key(self.vars, id)
        let self.vars[id] = { 'instances' : [] }
    endif
    call add(self.vars[id].instances, a:var)
endfunction

function! s:parser_var() dict
    let ret = []
    if self.same('{')
        let id = self.id()
        if id >= 0
            call add(ret, id)
            call extend(ret, self.varend())
        endif
    else
        let id = self.id()
        if id >= 0
            call add(ret, id)
        endif
    endif
    return ret
endfunction

function! s:parser_varend() dict
    let ret = []
    if self.same(':')
        call extend(ret, self.placeholder())
    elseif self.same('/')
        call add(ret, self.subst())
    endif
    call self.same('}')
    return ret
endfunction

function! s:parser_placeholder() dict
    return self.parse('}')
endfunction

function! s:parser_subst() dict
    let ret = {}
    if self.same('/')
        let ret.flags = 'g'
    endif
    let ret.pat = join(self.text('/', 1))
    if self.same('/')
        let ret.sub =  join(self.text('}', 1))
    endif
    return ret
endfunction

function! s:parser_expr() dict
    let str = join(self.text('`', 1))
    let ret = string(eval(str))
    call self.same('`')
    return ret
endfunction

function! s:parser_text(...) dict
    let res = []
    let val = ''
    if a:0 == 2 && a:2
        let till = '\V' . escape(a:1, '\')
    else
        let till = '[`$' . (a:0 ? a:1 : '') . ']'
    endif

    while self.pos < self.len
        if self.same('\')
            let val .= self.next
            call self.advance()
        elseif self.next =~# till
            break
        elseif self.next == "\n"
            call add(res, val)
            let val = ''
            call self.advance()
        elseif self.next == "\t" && &expandtab
            let val .= repeat(' ', (&sts > 0) ? &sts : &sw)
            call self.advance()
        else
            let val .= self.next
            call self.advance()
        endif
    endwhile

    call add(res, val)
    return res
endfunction

function! s:parser_parse(...) dict
    let ret = []
    while self.pos < self.len
        if self.same('$')
            let var = self.var()
            if var[0] is# 'VISUAL'
                call add(ret, s:visual_placeholder(var))
            elseif var[0] >= 0
                call add(ret, var)
                call self.add_var(var)
            endif
        elseif self.same('`')
            let expr = self.expr()
            if type(ret[-1]) == type('')
                let ret[-1] .= expr
            else
                call add(ret, expr)
            endif
        else
            let text = a:0 ? self.text(a:1) : self.text()
            if exists('expr')
                let ret[-1] .= text[0]
                call remove(text, 0)
                unlet expr
            endif
            call extend(ret, text)
        endif
        if a:0 && self.next == a:1
            break
        endif
    endwhile
    return ret
endfunction

call s:add_methods('parser', [ 'advance', 'same', 'id', 'add_var', 'var', 'varend', 'placeholder', 'subst', 'expr', 'text', 'parse' ])

function! s:visual_placeholder(var)
    let dict = get(a:var, 1, {})
    let pat = get(dict, 'pat', '')
    let sub = get(dict, 'sub', '')
    let flags = get(dict, 'flags', '')
    let ret = substitute(get(b:, 'snipmate_visual', ''), pat, sub, flags)
    unlet! b:snipmate_visual
    return ret
endfunction

function! snipmate#parse#snippet(text)
    let parser = s:new_parser(a:text)
    let result = parser.parse()
    return [result, parser.vars]
endfunction
