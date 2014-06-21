" config which can be overridden (shared lines)
if !exists('g:snipMate')
  let g:snipMate = {}
endif

try
	call tlib#input#List('mi', '', [])
catch /.*/
	echoe "you're missing tlib. See install instructions at ".expand('<sfile>:h:h').'/README.md'
endtry

" match $ which doesn't follow a \
let s:d = '\%([\\]\@<!\$\)'

fun! Filename(...)
	let filename = expand('%:t:r')
	if filename == '' | return a:0 == 2 ? a:2 : '' | endif
	return !a:0 || a:1 == '' ? filename : substitute(a:1, '$1', filename, 'g')
endf

let s:cache = {}

function! snipMate#expandSnip(snip, col)
	let lnum = line('.')
	let col = a:col
	let line = getline(lnum)
	let indent = match(line, '\S\|$') + 1
	let b:snip_state = snipmate#jumping#state()
	let [snippet, b:snip_state.stops] = snipmate#parse#snippet(a:snip)

	" Abort if the snippet is empty
	if empty(snippet)
		return ''
	endif

	" Build stop/mirror info
	let b:snip_state.stop_count = s:build_stops(snippet, b:snip_state.stops, lnum, col, indent)

	" Expand snippet onto current position
	let snipLines = snipMate#sniplist_str(snippet, b:snip_state.stops)
	let afterCursor = strpart(line, col - 1)
	" Keep text after the cursor
	if afterCursor != "\t" && afterCursor != ' '
		let line = strpart(line, 0, col - 1)
		let snipLines[-1] .= afterCursor
	else
		let afterCursor = ''
		" For some reason the cursor needs to move one right after this
		if line != '' && col == 1 && &ve != 'all' && &ve != 'onemore'
			let col += 1
		endif
	endif

	" Insert snippet with proper indentation
	call setline(lnum, line . snipLines[0])
	call append(lnum, map(snipLines[1:], "empty(v:val) ? v:val : '" . strpart(line, 0, indent - 1) . "' . v:val"))

	" Open any folds snippet expands into
	if &foldenable
		silent! exec lnum . ',' . (lnum + len(snipLines) - 1) . 'foldopen'
	endif

	aug snipmate_changes
		au CursorMoved,CursorMovedI <buffer> if exists('b:snip_state') |
					\     call b:snip_state.update_changes() |
					\ else |
					\     silent! au! snipmate_changes * <buffer> |
					\ endif
	aug END

	let b:snip_state.stop_no = 0
	return b:snip_state.set_stop(0)
endfunction

function! snipMate#placeholder_str(num, stops)
	return snipMate#sniplist_str(a:stops[a:num].placeholder, a:stops)[0]
endfunction

function! snipMate#sniplist_str(snippet, stops)
	let lines = ['']
	let pos = 0
	let add_to = 1
	let seen_stops = []

	while pos < len(a:snippet)
		let item = a:snippet[pos]

		if type(item) == type('')
			if add_to
				let lines[-1] .= item
			else
				call add(lines, item)
			endif
			let add_to = 0
		elseif type(item) == type([])
			let lines[-1] .= snipMate#placeholder_str(item[0], a:stops)
			let add_to = 1
		endif

		let pos += 1
		unlet item " avoid E706
	endwhile

	return lines
endfunction

function! s:build_stops(snippet, stops, lnum, col, indent)
	let stops = a:stops
	let line  = a:lnum
	let col   = a:col

	for [id, dict] in items(stops)
		for i in dict.instances
			if len(i) > 1 && type(i[1]) != type({})
				if !has_key(dict, 'placeholder')
					let dict.placeholder = i[1:]
				else
					unlet i[1:]
				endif
			endif
		endfor
		if !has_key(dict, 'placeholder')
			let dict.placeholder = []
			let j = 0
			while len(dict.instances[j]) > 1
				let j += 1
			endwhile
			call add(dict.instances[j], '')
		endif
		unlet dict.instances
	endfor

	let [line, col] = s:build_loc_info(a:snippet, stops, line, col, a:indent)

	" add zero tabstop if it doesn't exist and then link it to the highest stop
	" number
	let stops[0] = get(stops, 0,
				\ { 'placeholder' : [], 'line' : line, 'col' : col })
	let stop_count = max(keys(stops)) + 2
	let stops[stop_count - 1] = stops[0]

	return stop_count
endfunction

function! s:build_loc_info(snippet, stops, line, col, indent)
	let stops   = a:stops
	let line    = a:line
	let col     = a:col
	let pos     = 0
	let in_text = 0

	while pos < len(a:snippet)
		let item = a:snippet[pos]

		if type(item) == type('')
			if in_text
				let line += 1
				let col = a:indent
			endif
			let col += len(item)
			let in_text = 1
		elseif type(item) == type([])
			let id = item[0]
			if len(item) > 1 && type(item[1]) != type({})
				let stops[id].line = line
				let stops[id].col = col
				let [line, col] = s:build_loc_info(item[1:], stops, line, col, a:indent)
			else
				call s:add_mirror(stops, id, line, col, item)
				let col += len(snipMate#placeholder_str(id, stops))
			endif
			let in_text = 0
		endif

		let pos += 1
		unlet item " avoid E706
	endwhile

	return [line, col]
endfunction

function! s:add_mirror(stops, id, line, col, item)
	let stops = a:stops
	let item = a:item
	let stops[a:id].mirrors = get(stops[a:id], 'mirrors', [])
	let mirror = get(a:item, 1, {})
	let mirror.line = a:line
	let mirror.col = a:col
	call add(stops[a:id].mirrors, mirror)
	if len(item) == 1
		call add(item, mirror)
	endif
endfunction

" reads a .snippets file
" returns list of
" ['triggername', 'name', 'contents']
" if triggername is not set 'default' is assumed
fun! snipMate#ReadSnippetsFile(file)
	let result = []
	let new_scopes = []
	if !filereadable(a:file) | return [result, new_scopes] | endif
	let inSnip = 0
	for line in readfile(a:file) + ["\n"]
		if inSnip && (line[0] == "\t" || line == '')
			let content .= strpart(line, 1)."\n"
			continue
		elseif inSnip
			call add(result, [trigger, name == '' ? 'default' : name, content[:-2]])
			let inSnip = 0
		endif

		if line[:6] == 'snippet'
			let inSnip = 1
			let trigger = strpart(line, 8)
			let name = ''
			let space = stridx(trigger, ' ') + 1
			if space " Process multi snip
				let name = strpart(trigger, space)
				let trigger = strpart(trigger, 0, space - 1)
			endif
			let content = ''
		elseif line[:6] == 'extends'
			call extend(new_scopes, map(split(strpart(line, 8)),
						\ "substitute(v:val, ',*$', '', '')"))
		endif
	endfor
	return [result, new_scopes]
endf

function! s:GetScopes()
	let ret = exists('b:snipMate_scope_aliases') ? copy(b:snipMate.scope_aliases) : {}
	let global = get(g:snipMate, 'scope_aliases', {})
	for alias in keys(global)
		if has_key(ret, alias)
			let ret[alias] = join(split(ret[alias], ',')
						\ + split(global[alias], ','), ',')
		else
			let ret[alias] = global[alias]
		endif
	endfor
	return ret
endfunction

" adds scope aliases to list.
" returns new list
" the aliases of aliases are added recursively
fun! s:AddScopeAliases(list)
  let did = {}
  let scope_aliases = s:GetScopes()
  let new = a:list
  let new2 =  []
  while !empty(new)
	for i in new
	  if !has_key(did, i)
		let did[i] = 1
		call extend(new2, split(get(scope_aliases,i,''),','))
	  endif
	endfor
	let new = new2
	let new2 = []
  endwhile
  return keys(did)
endf

if v:version >= 704
	function! s:Glob(path, expr)
		return split(globpath(a:path, a:expr), "\n")
	endfunction
else
	function! s:Glob(path, expr)
		let res = []
		for p in split(a:path, ',')
			let h = split(fnamemodify(a:expr, ':h'), '/')[0]
			if isdirectory(p . '/' . h)
				call extend(res, split(glob(p . '/' . a:expr), "\n"))
			endif
		endfor
		return filter(res, 'filereadable(v:val)')
	endfunction
endif

" returns dict of
" { path: { 'type': one of 'snippet' 'snippets',
"           'exists': 1 or 0
"           " for single snippet files:
"           'name': name of snippet
"           'trigger': trigger of snippet
"         }
" }
" use mustExist = 1 to return existing files only
"
"     mustExist = 0 is used by OpenSnippetFiles
function! snipMate#GetSnippetFiles(mustExist, scopes, trigger)
	let paths = join(funcref#Call(g:snipMate.snippet_dirs), ',')
	let result = {}
	let scopes = s:AddScopeAliases(a:scopes)
	let trigger = escape(a:trigger, "*[]?{}`'$")

	" collect existing files
	for scope in scopes

		for f in s:Glob(paths, 'snippets/' . scope . '.snippets') +
					\ s:Glob(paths, 'snippets/' . scope . '_*.snippets') +
					\ s:Glob(paths, 'snippets/' . scope . '/*.snippets')
			let result[f] = { 'exists' : 1, 'type' : 'snippets',
						\ 'name_prefix' : fnamemodify(f, ':t:r') }
		endfor

		" We check for trigger* in the next two loops. In the case of an exact
		" match, that'll be handled in snipMate#GetSnippetsForWordBelowCursor.
		for f in s:Glob(paths, 'snippets/' . scope . '/' . trigger . '*.snippet')
			let result[f] = {'exists': 1, 'type': 'snippet', 'name': 'default',
						\ 'trigger': fnamemodify(f, ':t:r'), 'name_prefix' : scope }
		endfor

		for f in s:Glob(paths, 'snippets/' . scope . '/' . trigger . '*/*.snippet')
			let result[f] = {'exists': 1, 'type': 'snippet', 'name' : fnamemodify(f, ':t:r'),
						\ 'trigger': fnamemodify(f, ':h:t'), 'name_prefix' : scope }
		endfor

		if !a:mustExist
			for p in split(paths, ',')
				let p .= '/snippets/' . scope . '.snippets'
				let result[p] = get(result, p, {'exists': 0, 'type': 'snippets'})
			endfor
		endif

	endfor
	return result
endfunction

" should be moved to utils or such?
function! snipMate#SetByPath(dict, trigger, path, snippet)
	let d = a:dict
	if !has_key(d, a:trigger)
		let d[a:trigger] = {}
	endif
	let d[a:trigger][a:path] = a:snippet
endfunction

function! s:CachedSnips(file)
	let mtime = getftime(a:file)
	if has_key(s:cache, a:file) && s:cache[a:file].mtime >= mtime
		return s:cache[a:file].contents
	endif
	let s:cache[a:file] = {}
	let s:cache[a:file].mtime = mtime
	let s:cache[a:file].contents = snipMate#ReadSnippetsFile(a:file)
	return s:cache[a:file].contents
endfunction

" default triggers based on paths
function! snipMate#DefaultPool(scopes, trigger, result)
	let extra_scopes = []
	for [f,opts] in items(snipMate#GetSnippetFiles(1, a:scopes, a:trigger))
		let opts.name_prefix = matchstr(f, '\v/\zs.{-}\ze/snippets') . ' ' . opts.name_prefix
		if opts.type == 'snippets'
			let [snippets, new_scopes] = s:CachedSnips(f)
			call extend(extra_scopes, new_scopes)
			for [trigger, name, contents] in snippets
				if trigger =~ '\V\^' . escape(a:trigger, '\')
					call snipMate#SetByPath(a:result, trigger,
								\ opts.name_prefix . ' ' . name, contents)
				endif
			endfor
		elseif opts.type == 'snippet'
			call snipMate#SetByPath(a:result, opts.trigger,
						\ opts.name_prefix . ' ' . opts.name, readfile(f))
		else
			throw "unexpected"
		endif
	endfor

	if !empty(extra_scopes)
		call snipMate#DefaultPool(extra_scopes, a:trigger, a:result)
	endif
endfunction

" return a dict of snippets found in runtimepath matching trigger
" scopes: list of scopes. usually this is the filetype. eg ['c','cpp']
" trigger may contain glob patterns. Thus use '*' to get all triggers
"
fun! snipMate#GetSnippets(scopes, trigger)
	let result = {}

	for F in values(g:snipMateSources)
	  call funcref#Call(F, [a:scopes, a:trigger, result])
	endfor
	return result
endf

" adds leading tab
" and replaces leading spaces by tabs
" see ftplugin/snippet.vim
fun! snipMate#RetabSnip() range
  let leadingTab = expand('%:e') == 'snippets'

  let lines = getline(a:firstline, a:lastline)

  " remove leading "\t"
  let allIndented = 1
  for l in lines
	if l[0] != '\t' | let allIndented = 0 | endif
  endfor

  " retab
  if allIndented
	call map(lines, 'v:val[1:]')
  endif

  let leadingSp = filter(map(copy(lines),'matchstr(v:val,"^\\s*") '),'v:val !=""')
  if !empty(leadingSp)
	" lines containing leading spaces found
	let smallestInd =  len(sort(leadingSp)[-1])
	let ind = input('retab, spaces per tab: ', smallestInd)
	for i in range(0, len(lines)-1)
	  let ml = matchlist(lines[i], '^\(\s*\)\(.*\)')
	  let lines[i] = repeat("\t", len(ml[1]) / ind)
				 \ . repeat( " ", len(ml[1]) % ind)
				 \ . ml[2]
	endfor
  endif
  " readd tab
  let tab = leadingTab ? "\t" : ""
  for i in range(0,len(lines)-1)
	call setline(a:firstline + i, tab.lines[i])
  endfor
endf

fun! snipMate#OpenSnippetFiles()
  let dict = snipMate#GetSnippetFiles(0, snipMate#ScopesByFile(), '*')
  " sort by files wether they exist - put existing files first
  let exists = []
  let notExists = []
  for [file, v] in items(dict)
	let v['file'] = file
	if v['exists']
	  call add(exists, v)
	else
	  call add(notExists, v)
	endif
  endfor
  let all = exists + notExists
  let show = map(copy(all),'(v:val["exists"] ? "exists:" : "does not exist yet:")." ".v:val["file"]')
  let select = tlib#input#List('mi', 'select files to be opened in splits', show)
  for idx in select
	exec 'sp '.all[idx - 1]['file']
  endfor
endf

fun! snipMate#ScopesByFile()
	" duplicates are removed in AddScopeAliases
	return filter(funcref#Call(g:snipMate.get_scopes), "v:val != ''")
endf

" used by both: completion and insert snippet
fun! snipMate#GetSnippetsForWordBelowCursor(word, exact)
	" Setup lookups: '1.2.3' becomes [1.2.3] + [3, 2.3]
	let parts = split(a:word, '\W\zs')
	if len(parts) > 2
		let parts = parts[-2:] " max 2 additional items, this might become a setting
	endif
	let lookups = [a:word]
	let lookup = ''
	for w in reverse(parts)
		let lookup = w . lookup
		if index(lookups, lookup) == -1
			call add(lookups, lookup)
		endif
	endfor

	" allow matching '.'
	if a:word =~ '\.$'
		call add(lookups, '.')
	endif

	" Remove empty lookup entries, but only if there are other nonempty lookups
	if len(lookups) > 1
		call filter(lookups, 'v:val != ""')
	endif

	let matching_snippets = []
	let snippet = ''
	" prefer longest word
	for word in lookups
		let g:snipMate.word = word
		for [k,snippetD] in items(funcref#Call(g:snipMate['get_snippets'], [snipMate#ScopesByFile(), word]))
			" hack: require exact match
			if a:exact && k !=# word
				continue
			endif
			call add(matching_snippets, [k, snippetD])
			if a:exact
				break
			endif
		endfor
	endfor
	return matching_snippets
endf

" snippets: dict containing snippets by name
" usually this is just {'default' : snippet_contents }
fun! s:ChooseSnippet(snippets)
	let snippet = []
	let keys = keys(a:snippets)
	let i = 1
	for snip in keys
		let snippet += [i.'. '.snip]
		let i += 1
	endfor
	if len(snippet) == 1
		" there's only a single snippet, choose it
		let idx = 0
	else
		let idx = tlib#input#List('si','select snippet by name',snippet) -1
		if idx == -1
			return ''
		endif
	endif
	" if a:snippets[..] is a String Call returns it
	" If it's a function or a function string the result is returned
	return funcref#Call(a:snippets[keys(a:snippets)[idx]])
endf

fun! snipMate#WordBelowCursor()
	return matchstr(getline('.'), '\S\+\%' . col('.') . 'c')
endf

fun! snipMate#GetSnippetsForWordBelowCursorForComplete(word)
	let snippets = map(snipMate#GetSnippetsForWordBelowCursor(a:word, 0), 'v:val[0]')
	return filter(snippets, 'v:val =~# "\\V\\^' . escape(a:word, '"\') . '"')
endf

fun! snipMate#CanBeTriggered()
	let word    = snipMate#WordBelowCursor()
	let matches = snipMate#GetSnippetsForWordBelowCursorForComplete(word)
	return len(matches) > 0
endf

fun! snipMate#ShowAvailableSnips()
	let col     = col('.')
	let word    = snipMate#WordBelowCursor()
	let matches = snipMate#GetSnippetsForWordBelowCursorForComplete(word)

	" Pretty hacky, but really can't have the tab swallowed!
	if len(matches) == 0
		call feedkeys(g:snipMate['no_match_completion_feedkeys_chars'], 'n')
		return ""
	endif

	call complete(col - len(word), sort(matches))
	return ''
endf

" Pass an argument to force snippet expansion instead of triggering or jumping
function! snipMate#TriggerSnippet(...)
	if exists('g:SuperTabMappingForward')
		if g:SuperTabMappingForward == "<tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabForward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-n>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		elseif g:SuperTabMappingBackward == "<tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabBackward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-p>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		endif
	endif

	if pumvisible() " Update snippet if completion is used, or deal with supertab
		if exists('SuperTabKey')
			call feedkeys(SuperTabKey) | return ''
		endif
		call feedkeys("\<esc>a", 'n') " Close completion menu
		call feedkeys("\<tab>") | return ''
	endif

	if exists('b:snip_state') && a:0 == 0 " Jump only if no arguments
		let jump = b:snip_state.jump_stop(0)
		if type(jump) == 1 " returned a string
			return jump
		endif
	endif

	let word = matchstr(getline('.'), '\S\+\%'.col('.').'c')
	let list = snipMate#GetSnippetsForWordBelowCursor(word, 1)
	if empty(list)
		let snippet = ''
	else
		let [trigger, snippetD] = list[0]

		let s = s:ChooseSnippet(snippetD)
		if type(s) == type([])
			let snippet = join(s, "\n")
		else
			let snippet = s
		end

		" Before expanding snippet, create new undo point |i_CTRL-G|
		let &undolevels = &undolevels
		let col = col('.') - len(trigger)
		sil exe 's/\V'.escape(trigger, '/\.').'\%#//'
		return snipMate#expandSnip(snippet, col)
	endif

	" should allow other plugins to register hooks instead (duplicate code)
	if exists('SuperTabKey')
		call feedkeys(SuperTabKey)
		return ''
	endif
	return word == ''
	  \ ? "\<tab>"
	  \ : "\<c-r>=snipMate#ShowAvailableSnips()\<cr>"
endfunction

fun! snipMate#BackwardsSnippet()
	if exists('b:snip_state') | return b:snip_state.jump_stop(1) | endif

	if exists('g:SuperTabMappingForward')
		if g:SuperTabMappingForward == "<s-tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabForward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-n>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		elseif g:SuperTabMappingBackward == "<s-tab>"
			let SuperTabPlug = maparg('<Plug>SuperTabBackward', 'i')
			if SuperTabPlug == ""
				let SuperTabKey = "\<c-p>"
			else
				exec "let SuperTabKey = \"" . escape(SuperTabPlug, '<') . "\""
			endif
		endif
	endif
	" should allow other plugins to register hooks instead (duplicate code)
	if exists('SuperTabKey')
		call feedkeys(SuperTabKey)
		return ''
	endif
	return "\<s-tab>"
endf

" vim:noet:sw=4:ts=4:ft=vim
