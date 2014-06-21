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

let s:state_proto = {}

function! snipmate#jumping#state()
	return copy(s:state_proto)
endfunction

function! s:listize_mirror(mirrors)
	return map(copy(a:mirrors), '[v:val.line, v:val.col]')
endfunction

" Removes snippet state info
function! s:state_remove() dict
	" Remove all autocmds in group snipmate_changes in the current buffer
	unlet! b:snip_state
	au! snipmate_changes * <buffer>
endfunction

" Update state information to correspond to the given tab stop
function! s:state_set_stop(backwards) dict
	let self.stop_no += a:backwards? -1 : 1
	while !has_key(self.stops, self.stop_no)
		if self.stop_no == self.stop_count
			let self.stop_no = 0
		endif
		if self.stop_no <= 0 && a:backwards
			let self.stop_no = self.stop_count - 1
		endif
		let self.stop_no += a:backwards? -1 : 1
	endwhile
	let self.cur_stop    = self.stops[self.stop_no]
	let self.stop_len    = len(snipMate#placeholder_str(self.stop_no, self.stops))
	let self.start_col   = self.cur_stop.col
	let self.end_col     = self.start_col + self.stop_len
	let self.mirrors     = get(self.cur_stop, 'mirrors', [])
	let self.old_mirrors = deepcopy(self.mirrors)
	call cursor(self.cur_stop.line, self.cur_stop.col)
	let self.prev_len    = col('$')
	let self.changed = 0
	let ret = self.select_word()
	if (self.stop_no == 0 || self.stop_no == self.stop_count - 1) && !a:backwards
		call self.remove()
	endif
	return ret
endfunction

" Jump to the next/previous tab stop
function! s:state_jump_stop(backwards) dict
	" Update changes just in case
	" This seems to be only needed because insert completion does not trigger
	" the CursorMovedI event
	call self.update_changes()

	" Update stop and mirror locations
	call self.update_stops()

	" Store placeholder/location changes
	let self.cur_stop.col = self.start_col
	if self.changed
		call self.remove_nested()
		let self.cur_stop.placeholder = [strpart(getline('.'),
					\ self.start_col - 1, self.end_col - self.start_col)]
	endif

	return self.set_stop(a:backwards)
endfunction

function! s:state_remove_nested(...) dict
	let id = a:0 ? a:1 : self.stop_no
	for i in self.stops[id].placeholder
		if type(i) == type([])
			if len(i) > 1 && type(i[1]) != type({})
				call self.remove_nested(i[0])
				call remove(self.stops, i[0])
			else
				call filter(self.stops[i[0]].mirrors, 'v:val is i')
			endif
		endif
		unlet i " Avoid E706
	endfor
endfunction

" Update tab stop/mirror locations to account for mirror changes
function! s:state_update_stops() dict
	let changeLen = self.end_col - self.stop_len - self.start_col
	let curLine = line('.')
	if changeLen != 0
		" Filter the zeroth stop because it's duplicated as the last
		for stop in values(filter(copy(self.stops), 'v:key != 0'))
			if stop is self.cur_stop
				continue
			endif

			let changed = stop.line == curLine && stop.col > self.cur_stop.col
			" Subtract changeLen from each tab stop that was after any of
			" the current tab stop's placeholders.
			for [lnum, col] in s:listize_mirror(self.old_mirrors)
				if lnum > stop.line
					break
				endif
				if stop.line == lnum
							\ && (stop.col > col
							\ || (stop.placeholder == [''] && stop.col == col))
					let changed += 1
				endif
			endfor
			let stop.col += changeLen * changed

			" Do the same to any placeholders in the other tab stops.
			for mirror in get(stop, 'mirrors', [])
				let changed = mirror.line == curLine && mirror.col > self.start_col
				if changed && mirror.col < self.start_col + self.cur_stop.col
					call remove(stop.mirrors, index(stop.mirrors, mirror))
					continue
				endif
				for [lnum, col] in s:listize_mirror(self.old_mirrors)
					if lnum > mirror.line
						break
					endif
					if mirror.line == lnum && mirror.col > col
						let changed += 1
					endif
				endfor
				let mirror.col += changeLen * changed
			endfor
		endfor
	endif
endfunction

" Select the placeholder for the current tab stop
function! s:state_select_word() dict
	let len = self.stop_len
	if !len | return '' | endif
	let l = col('.') != 1 ? 'l' : ''
	if &sel == 'exclusive'
		return "\<esc>".l.'v'.len."l\<c-g>"
	endif
	return len == 1 ? "\<esc>".l.'gh' : "\<esc>".l.'v'.(len - 1)."l\<c-g>"
endfunction

" Update the snippet as text is typed. The self.update_mirrors() function does
" the actual work.
" If the cursor moves outside of a placeholder, call self.remove()
function! s:state_update_changes() dict
	let change_len = col('$') - self.prev_len
	let self.changed = self.changed || change_len != 0
	let self.end_col += change_len
	let col = col('.')

	if line('.') != self.cur_stop.line || col < self.start_col || col > self.end_col
		return self.remove()
	elseif !empty(self.mirrors)
		call self.update_mirrors(change_len)
	endif

	let self.prev_len = col('$')
endfunction

" Actually update the mirrors for any changed text
function! s:state_update_mirrors(change) dict
	let newWordLen = self.end_col - self.start_col
	let newWord = strpart(getline('.'), self.start_col - 1, newWordLen)
	let changeLen = a:change
	let curLine = line('.')
	let curCol = col('.')
	let oldStartSnip = self.start_col
	let i = 0

	for mirror in self.mirrors
		if changeLen != 0
			let start = self.start_col
			if mirror.line == curLine && mirror.col <= start
				let self.start_col += changeLen
				let self.end_col += changeLen
			endif
			for nPos in self.mirrors[(i):]
				" This list is in ascending order, so quit if we've gone too far.
				if nPos.line > mirror.line
					break
				endif
				if nPos.line == mirror.line && nPos.col > mirror.col
					let nPos.col += changeLen
				endif
			endfor
			if mirror.line == curLine && mirror.col > start
				let mirror.col += changeLen
			endif
			let i += 1
		endif

		" Split the line into three parts: the mirror, what's before it, and
		" what's after it. Then combine them using the new mirror string.
		" Subtract one to go from column index to byte index
		let theline = getline(mirror.line)
		let update  = strpart(theline, 0, mirror.col - 1)
		let update .= substitute(newWord, get(mirror, 'pat', ''), get(mirror, 'sub', ''), get(mirror, 'flags', ''))
		let update .= strpart(theline, mirror.col + self.end_col - self.start_col - a:change - 1)
		call setline(mirror.line, update)
	endfor

	" Reposition the cursor in case a var updates on the same line but before
	" the current tabstop
	if oldStartSnip != self.start_col || mode() == 'i'
		call cursor(0, curCol + self.start_col - oldStartSnip)
	endif
endfunction

call s:add_methods('state', [ 'remove', 'set_stop', 'jump_stop',
			\ 'remove_nested', 'update_stops', 'select_word', 'update_changes',
			\ 'update_mirrors' ])

" vim:noet:sw=4:ts=4:ft=vim
