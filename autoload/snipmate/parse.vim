" Snippet definition parsing code

function! s:new_stack(...)
    let stack = { 'items' : [] }

    function! stack.push(item) dict
        call add(self.items, a:item)
    endfunction

    function! stack.top() dict
        return get(self.items, -1)
    endfunction

    function! stack.pop() dict
        if !empty(self.items)
            return remove(self.items, -1)
        endif
    endfunction

    if a:0
        call stack.push(a:1)
    endif

    return stack
endfunction

function! snipmate#parse#snippet(text)
    if type(a:text) == type([])
        let text = join(a:text, "\n")
    else
        let text = a:text
    endif

    let state_stack = s:new_stack('none')
    let pos = 0
    let result = []
    let stops = {}
    let target_stack = s:new_stack(result)
    let contain_stack = s:new_stack([])
    let stop_proto = { 'contains' : [], 'placeholder' : [''] }

    while pos < strlen(a:text)
        let cur_state = state_stack.top()
        let cur_target = target_stack.top()
        let cur_contain = contain_stack.top()
        let [tok_type, tok_val, pos] = s:next_token(a:text, pos)

        if cur_state == 'expr' && tok_type == 'expr'
            call state_stack.pop()
            let str = join(target_stack.pop(), '')
            call add(target_stack.top(), string(eval(str)))
        elseif cur_state == 'mirror' && tok_type == 'id'
            call add(cur_target, +tok_val)
            call add(cur_contain, +tok_val)
            call state_stack.pop()
        elseif cur_state == 'stop' && tok_type == 'id'
            let num = +tok_val
            call add(cur_target, num)
            call add(cur_contain, num)
            let stops[num] = deepcopy(stop_proto)
            let [tok_type, tok_val, pos] = s:next_token(a:text, pos)
            if tok_type == 'placeholder'
                call state_stack.push('placeholder')
                call contain_stack.push(stops[num].contains)
            elseif tok_type == 'stop end'
                call add(cur_target, '')
                call state_stack.pop()
                call target_stack.pop()
            else
                call state_stack.pop()
                call target_stack.pop()
            endif
        elseif cur_state == 'stop' && tok_type == 'visual'
            let [tok_type, tok_val, pos] = s:next_token(a:text, pos)
            if tok_type == 'stop end'
                call target_stack.pop()
                let cur_target = target_stack.top()
                call remove(cur_target, -1)
                let b:snipmate_visual = get(b:, 'snipmate_visual', '')
                call add(cur_target, b:snipmate_visual)
                unlet b:snipmate_visual
            else
                call target_stack.pop()
            endif
            call state_stack.pop()
        elseif cur_state == 'placeholder'
                    \ && tok_type == 'stop end'
            call state_stack.pop()
            call state_stack.pop()
            let stops[cur_target[0]].placeholder = cur_target[1:]
            call target_stack.pop()
            call contain_stack.pop()
        elseif cur_state == 'none' || cur_state == 'placeholder'
            if tok_type == 'stop start'
                call add(cur_target, [])
                call target_stack.push(cur_target[-1])
                call state_stack.push('stop')
            elseif tok_type == 'mirror'
                call state_stack.push('mirror')
            elseif tok_type == 'expr'
                let cur_target = []
                call target_stack.push(cur_target)
                call state_stack.push('expr')
            else
                call add(cur_target, tok_val)
            endif
        elseif cur_state == 'expr'
            call add(cur_target, tok_val)
        else
            call state_stack.pop()
        endif
    endwhile

    return [s:format_text(result), stops]
endfunction

function! s:next_token(text, pos)
    let pos = a:pos
    let val = ''

    while pos < strlen(a:text)
        if a:text[pos] == '\' && a:text[pos + 1] =~ '[\$`}]'
            let val .= a:text[pos + 1]
            let pos += 2
        elseif a:text[pos] =~ '[`${:}0-9]' && !empty(val)
            return ['text', val, pos]
        elseif a:text[pos] == '$' " stop or mirror
            if a:text[pos + 1] == '{'
                return ['stop start', '${', pos + 2]
            else
                return ['mirror', '$', pos + 1]
            endif
        elseif a:text[pos] == '`'
            return ['expr', '`', pos + 1]
        elseif a:text[pos] == ':'
            return ['placeholder', ':', pos + 1]
        elseif a:text[pos] == '}'
            return ['stop end', '}', pos + 1]
        elseif a:text[(pos):(pos+5)] == 'VISUAL'
            return ['visual', 'VISUAL', pos + 6]
        elseif a:text[pos] =~ '\d'
            let end = matchend(a:text, '\d\+', pos)
            return ['id', strpart(a:text, pos, end - pos), end]
        elseif a:text[pos] == "\t" && &expandtab
            let val .= repeat(' ', (&sts > 0) ? &sts : &sw)
            let pos += 1
        else
            let val .= a:text[pos]
            let pos += 1
        endif
    endwhile

    return ['eof', val, pos]
endfunction

" Join consecutive pieces of text and split text according to lines
function! s:format_text(sniplist)
    let pos = 0
    let sniplist = a:sniplist

    while pos < len(sniplist)
        if type(sniplist[pos]) == type('')
            let i = 0
            let text = ''
            while pos + i < len(sniplist)
                        \ && type(sniplist[pos + i]) == type('')
                let text .= sniplist[pos + i]
                let i += 1
            endwhile
            call remove(sniplist, pos, pos + i - 1)
            let pos += s:splice(sniplist, split(text, "\n", 1), pos)
        elseif type(sniplist[pos]) == type([])
            call s:format_text(sniplist[pos])
            let pos += 1
        else
            let pos += 1
        endif
    endwhile

    return sniplist
endfunction

function! s:splice(target, other, idx)
    let idx = a:idx
    for i in a:other
        call insert(a:target, i, idx)
        let idx += 1
    endfor
    return len(a:other)
endfunction
