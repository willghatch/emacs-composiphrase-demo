# Composiphrase, the composable modal editor, the demo configuration


## The overall idea

This Composiphrase demo configuration is a modal editor, like Vim.
There are different modes, called states in Composiphrase.
In insert state you type characters.
In “normal” state, keys execute commands.
However, most keys don't do anything alone.
Keys are composed together to create and execute command sentences.
The composition allows a large number of commands to be used, and, equally importantly, memorized, using a small number of keys.

A command sentence always has a verb, an object, and may have additional modifiers.
However, the verb may be omitted, with “move” as the default verb, and modifiers have default values depending on the verb and object.
When moving, the `h` and `l` keys mean backwards, and forwards, respectively, and `a` means selection (mnemonic: select “a” thing).
After pressing one of those, the next key selects an object.
So `lc` moves forward one character, like `l` in vim.
`ht` moves backward one sibling in the treesitter tree.
`le` moves forward one sibling in the indentation tree.
`aw` selects the word that the cursor is at.
Etc.
There are a ton of different objects, way more than Vim supports.
(Also note, unlike Vim, since Composiphrase always uses between-cursor positioning, `a` is selection in the normal state map and the visual state map.)

Modifiers, in this demo, all live in the object key map.
So they can be selected after choosing forward or backward.
For example, `hus` moves backward `up` to the open parenthesis of the s-expression tree, and `lnl` moves forward to the `end` of the line.

Moving beyond “move”, there are many other verbs.
Access them with the `s` key at the start of a command.
So `sd` is delete (though, to be consistent with vim, this demo also leaves it on the root map as just `d`).
Delete takes a movement or selection, so `sdlw` (or `dlw`) deletes forward to the start of the next word, and `sdluis` deletes forward to the inside of the close parenthesis of the current s-expression (`u` up and `i` inner), and `sdae` selects and deletes the current indentation tree.
Composiphrase has the verbs that vim supports (eg. move, delete, change, yank), as well as some where vim supports it in a non-composable way (eg. open, where vim has open line), as well as a slew of others.
So `sols` opens a new s-expression forward, a sibling of the one at point, with the cursor left in insert state, and `soldo` opens a new outline (org-mode) heading child of the current heading (child because it added the `d` down modifier).

Like Vim, a common modifier is a number.
You can write the number at any point before the object.
Typically the number indicates the number of moves for commands that operate on a region based on movement (like delete, change), or number of times to do something (for transpose, slurp), or some detail of the operation (eg. `2soldo` opens a new child heading at index 2).
Honestly, I don't use number modifiers very often, I more often repeat things.
See the section on repeating below.

Also, for commands that operate on a region, it is often easier to enter visual state first (with the `v` key), do the movement, then select the command.
These commands execute on the region implicitly in visual state, rather than waiting for the sentence to end with an object.

Because Composiphrase is implemented in Emacs, all normal Emacs functionality is available.
For example, Meta-x (or Alt-x) is always available to type the name of an Emacs command.

Also note that the keys shown here are a demonstration configuration.
You can re-bind the keys to add different pieces to the current command sentence, and change the rules about when the sentence is considered complete and executed (eg. including having an execute key, separate from executing after any particular sentence state).
And, orthogonally, you can redesign how verbs, objects, and modifiers are matched to execute different concrete commands, and/or add your own verbs, objects, and modifiers.

For the price of slightly longer “sentences”, compared to Vim, Composiphrase puts an order of magnitude more commands at your fingertips, and makes it easier to memorize than the commands of Vim.
The extra key strokes are worth the extra command availability and fluency.

For other thoughts about the overall idea, see the [announcement blog post](https://willghatch.net/blog/text-editing/composiphrase_composable-editing-language-like-vim-but-moreso/).


## Limitations

This Composiphrase demo setup is “demo-ware”.
Not only does it have no promise of stability, but it's something I cobbled together in a hurry to finish it on a time budget.
(But keep reading -- it's still worth a try or at least a look.)

I'm going to keep documenting things as if it were complete, and not full of holes and missing functionality.
I want you to be able to catch the vision of what it can be.
But I'll try to note what does and doesn't actually work.
The dagger † will indicate completely missing functionality.
The shrug emoji 🤷 will indicate known bugs or limitations in an existing implementation.
But there are many ways of combining things, and I don't have the time or inclination right now to exhaustively document each composition and whether it works.


## Objects

Objects have varying levels of support for different verbs.
Virtually all objects support movement, selection, the verbs that use a region based on movement or selection, and transposition.
Many objects are trees, and support vertical movement, region expansion by repeated selection, and tree-related verbs like slurp, barf, demote, and vertical transposition.



### Trees generally

Not a specific object, but for all trees, note that the `up`, `down`, and `inner` modifiers are useful.
`inner` with selection selects all of the children of the tree without the parent, such as selecting everything but the parentheses, or selecting all headings underneath a particular org heading, etc.
`inner` plus movement backward goes to the start of the child area (eg. inside the parenthesis), `inner` plus movement forward goes inside the end parenthesis where applicable.
`select` plus `up` selects the root-level tree (well, arguably the root would be the whole buffer, in which case it is next to the root).
`transpose` plus `up` reorders ancestor nodes in the tree.
Trees also support in-order traversal with the `inorder` modifier, though, ... it's not very useful.
Forward/backward motion within trees moves to sibling nodes, but does not move past the first/last sibling by default (IE `respect-tree` modifier sets this behavior).
(I like this because it is helpful to explore where the boundaries of trees are, and have the editor confirm my belief that something is the end of a tree in cases of visual confusion, etc.)
Movement with the `absolute` modifier in trees goes to the Nth sibling, and negative numbers are supported to go to the Nth from the end†.

### character

`c` in object map.
Single character.  Moving to the start or the end of a character is the same.
With `absolute` modifier, go to column.
With `absolute` and `alternate` modifiers, go to buffer offset.
With `absolute` modifier, the `forward` and `backward` modifiers make it default to first/last positions.

The object map also has `f` for character with `specific` modifier, which is probably a dumb encoding, but it then waits for you to type a specific character, and then moves to that character.
Similar (but not identical) to `f` key in vim.
The `end` modifier IS effective for moving to a specific character.
If you select with `f` (`af` then a character), it selects a region between two instances of that character†.
With `inner` modifier, that selection does not include the instances of the character†.

People talk about using `hjkl` in Vim.
But `h` and `l` in Vim move by single characters, and are inefficient.
Only do that when you are really just one or two characters off.
One of the best things I did when learning Vim was to unbind `h` and `l` to force myself to learn other movements.
Although I then overused the `w` and `b` word movements...
But really, it is worth it to get used to bigger movements, and especially tree movements.
I recommend that if you need to go to a specific distant place, start by using search (or maybe something like [Avy](https://github.com/abo-abo/avy)) to move, but then use structural motions like with smartparens, indent-tree, treesitter, etc.
When you get used to structural movements, and composing them with deletion, changing, selection, transposition, slurping, etc, and also repeating those things (with `.` or with keyboard macros), it can make many edits a breeze.

### word

`w` in object map.
This is the emacs definition of word, which is different from the word definition in vi.

### cpo-vi-like-word

I didn't bind this in the object map, because it's buggy.
I wrote it because I'm used to vim-like words.
I haven't decided whether to fix it, or try getting used to emacs-style words, or what.

Hypothetically, vi-like-word plus the `alternate` modifier gives vi-like-WORD.

### sentence

`S` in object map.
This is the emacs notion of sentence.
You can configure things like whether a sentence has two spaces after a period.

### paragraph

`P` in object map.
This is the emacs notion of paragraph.

### line

`l` in object map.
Note that in addition to normal forward/back motion by line to beginning (or end, with `end` modifier), the normal-state base map has `j` and `k` bound to move by line, but with some effort to keep the current column.
When selecting with `al`, it expands the current selection to include all of each line that point or mark are currently on, which means that repeated use of `al` will keep growing because the cursor after `al` is on the start of a blank line, and it will grow to include the next newline.
Use the `inner` modifier with selection to not include the final newline.
The `alternate` modifier, with `backward` motion, goes to the start of indentation on the current line.
This is not a very faithful encoding, I should maybe change it to go backward on any line, and require the `same-line` modifier, which I haven't really implemented anything with, yet.
Also, moving forward with the `alternate` modifier goes to before the trailing whitespace on the line†.

### symbol

`y` in object map.
This is a programming language identifier.
It is an emacs built-in concept, and changes according to the current major mode.

### cpo-smartparens

`s` in object map.
It is a tree, so all tree operations should work on it.

This is built on the [smartparens package](https://github.com/Fuco1/smartparens).
But note that the movements are Composiphrase-style, and the cursor positions for various operations are adjusted, and sibling motion respects the tree and does not jump out (unless you explicitly add the `disprespect-tree` modifier).

A smartparens sexp is a symbol (like the symbol object), or a number, or a parenthesized group, with configurable delimiters.

Also note that the object map has most delimiters bound to the smartparens object with the `delimiter` modifier set to the particular delimiter.
So eg. selecting like `a{` selects a sexp delimited specifically by braces.

Note that, as a tree, the `up`, `down`, and `inner` modifiers are all useful.

In practice, this is one of the most fleshed-out objects, almost everything that “should” work in composiphrase actually does work with the smartparens object.
Eg. slurp, barf, vertical transposition, etc. all work.
Note that some smartparens operations are actually not mapped at all, but could be added.
For example, replacing the parent sexp with the sexp at point is redundant with selecting the current sexp, copying, selecting again to select the parent sexp, then pasting, which is what I have the habit of doing.
Things like `sp-kill-sexp` are redundant with composing the `delete` verb with motions.
Also, `sp-convolute-sexp` is similar but not the same as `transpose` with the `up` modifier, and I haven't bound it.
You could reconfigure `transpose` `up` to be `sp-convolute-sexp`, or maybe it should be bound to `alternate` `transpose` `up`.


### sexp

`Y` in object map.
This is the emacs built-in notion of sexp.
I think it is mode-dependent.
But I never use it because I use smartparens.

Also, it doesn't support any actual tree stuff at the moment 🤷.

### list

`L` in the object map.
This is the emacs built-in notion of list, which is related to sexp.
I never use it because I use smartparens.
But maybe I should start using it, as it is a usefully different but overlapping thing with `sexp`.
Like the built-in `sexp` object, I think it is mode-dependent.
Also, it doesn't support any actual tree stuff at the moment 🤷.

### cpo-indent-tree

`e` in the object map.
This views the buffer as a tree based on how indented each line is.
As a tree, it supports the extra tree operations.

For example, in this buffer:

```
a
  aa
    aaa
    aab
  ab
    aba
    abc
  ac
b
  ba
  bb
  bc
```

If the cursor is on the `ab` line, then the current indent tree is the three lines:
```
  ab
    aba
    abc
```

Moving to parent, with eg. `hue` from there would move to the `a` line.

`inner` selections select all of the lines that are children of the parent line without selecting the parent line.

This text object is actually quite nice and rather versatile.
It is a useful low-fi view of syntax trees that works for all languages.
It is very useful with Python.

† Various things, of course, are not fully implemented.

### outline

`o` in object map.
Outline or org-mode tree.
As a tree, it supports the extra tree operations.
(† or, at least, I've implemented many of them.)
This is most useful in `org-mode`, but also works with `outline-minor-mode` in other modes.

### cpo-treesitter-qd

`t` in object map.
It is a tree, and thus supports the extra tree operations.
(† or, at least, some that I've implemented.)
It provides generic operations on treesitter trees.
Note that treesitter needs to be enabled in the buffer before this will work.
There are `X-ts` modes for different languages `X` that do this automatically, or you can manually enable treesitter using the `treesit` library.
I should maybe add code to make it enable treesitter automatically on first use...

In the future it might be better to have mode-specific handling to switch to more specific treesitter wrappers, and fall back to the generic wrapper if no specific wrapper is found.

Note that this generic treesitter wrapper has movements default to “anchor points” instead of beginnings.
These anchor points are typically symbol tokens or keywords.
With infix trees, for example, if you move to the beginning of an expression, it is often ambiguous which expression you are at the beginning of.
By using the infix operator, or other bits of syntax, we can treat specific buffer positions as uniquely belonging to a specific node in the tree.
However, this has its limits.

🤷 This generic handler also relies on keywords being in a particular list in the implementation.
TODO - make that list easily user-configurable, by, for example, looking up the name and writing it here.
I only bothered adding keywords from a couple of high profile languages so far.

### xml

`x` in object map.
As a tree, it supports extra tree operations.
† Except that I haven't implemented this at all.
It's probably just a simple matter of programming, wrapping the nxml package, for example.

Note that it is separate from the `xml-tag` object.
If the cursor is just before the start of an XML opening tag, moving `forward` `xml` will move just before the opening tag of the next element, or to a text element, and will not move to the closing tag, for example.
Elements of the XML element are entire trees, including both opening and closing tags.
Selecting with the `inner` modifier selects inside the tags, and `up` `inner` motion moves just inside the tags.

Note that the xml object works for XML embedded in strings, or inside JSX.
† Or, at least it should after it is implemented.

### xml-tag

`X` in object map.
Move to the next XML tag, opening or closing.
This is not a tree object, the `xml` object is a tree object.
† Except that I haven't implemented it at all.

### json

`j` in object map.
It is a tree.
Note that it works inside strings and such, inside files that are otherwise not json files.
† Except that I haven't implemented it at all.

### buffer

`B` in object map.
It is mostly useful for selection, which selects the whole buffer.
Forward movement goes to the end, backward goese to the beginning.
The encoding isn't very faithful to the rest of the system, but there's not much else to do.
I didn't want to make it switch buffers.
But maybe I should?

### url

`hu` in object map.
🤷 Useful for selection, but if you move forward/back to URL, it is super slow, because I just wrote the dumbest “move forward by a character then check if there is a URL at point”.  It needs an actual regex-based implementation to search forward or backward, but I didn't see that in emacs already and didn't want to implement it.  Also this is one of the few objects for which transposition is not implemented.

### email-address

`he` in object map.
🤷 Same caveats as with `url`.

### phone-number

`hP` in object map.
† Not implemented at all.

### tracking-number

† Not implemented, but you see the point here is that if there is any kind of thing that you can write a function to detect and move to, you can add objects to move to, select, and transpose, at a minimum.

### file-name
† See above.

### white-space

`h SPC` in object map.
Move to, or select, white space.
Or transpose white space, if that is somehow useful.
With `alternate` mod it ignores newlines.
(Or should that be the reverse?)
(† It doesn't matter much yet, because I haven't implemented it.)

### function-arg

`g` in object map.
† Not implemented.

But it should be specifically argument nodes in the syntax tree.
It can be quick and convenient to select or move to specific kinds of nodes in the tree.
Note that both values for the `respect-tree` modifier can be particularly useful for spsecific nodes.

### definition

`D` in object map.
See `function-arg`.
† Not implemented.

### comment
### statement
### class
### test

Ok, let's just group these.  All of these are specific nodes in the tree.
See `function-arg`.
† I haven't implemented any of these yet.

Bound to `C`, `M`, `hC`, `ht` in object map.

### buffer-change

`hc` in object map.
Move to, or select, change regions in the buffer.
† Not implemented.

### vcs-change

`hg` in object map.
Move to, or select, change regions as detected by eg. `git`.
† Not implemented.

### linter-warning

`hL` in object map.
Move to, or select, regions pointed to by linter or compiler warnings or errors.
† Not implemented.

### proposed-edit

`p` in object map.
Move to, or select, regions of proposed changes.
This could be useful for code review or using AI coding assistants.
† Not implemented.


### repeatable-motion-repeat

This is not truly an object, but is treated as an object so that repeated motions can compose with operations like delete that make ranges with movements.
It's not in the object map, it is `f` in the base normal-state map.
Reverse direction with `F`.

### isearch-new

Again, not a real object, but treated as one to compose with operations like delete.
🤷 Note that while isearch repeat works, if you do something like delete to search, then try to repeat it with ".", it is broken because my recording code doesn't work with recursive editing and I haven't fixed it yet.

### isearch-repeat

Again, not a real object, but treated as one to compose with region-based operations like delete.

### jump-to-register

Again, not a real object, but treated as one to compose with region-based operations like delete.

### region

The region object is not in the object map, but operations that operate on a region use the region (visual selection) as the object implicitly when in visual state, executing the command immediately.
Thus there is not a way at the moment to add modifiers to operations that take the region, but I'm also not certain what modifiers matter.
An exception is the register modifier, which has a way to access it (`"`) outside of the object map.



## Verbs

Note that some verbs are region-based, and thus can either operate on the current region (visual-state selection), or be composed with an object to get that region based on movement or selection.
All of these can take the same modifiers that the move operation can.
If the region is active, these are executed as soon as they are selected, using the `region` object instead of selecting one.

All verbs take an object, there is no verb in this system that doesn't compose with objects.
There are many useful commands that fit that mold, they are simply not included as Composiphrase verbs.
And I typically have not bound them in this demo keymap, except for a few core features that pair well with composiphrase, or seem necessary for practical use with composiphrase.

Some verbs only work on tree objects.


### move

The default verb, so with the demo key bindings you can skip giving a verb entirely, but it's also bound to `m` in the command map.
Use `h` and `l` to get into the object map and move forward or backward to the beginning of an object.
Or add `end` modifier with `n` to go to the end instead.
Or enter the object map with `a` to select.

### delete

`d` in command map, but also on base normal-state map as `d`.
Region-based.
Can take a register modifier to pick a register to save the deleted text in.

### change

`c` in command map, but also on base normal-state map as `c`.
Region-based.
Basically the same as delete, but puts you in insert state, so if you repeat it, you repeat both the deletion and the insert.
Can take a register modifier.

### copy

`y` in command map, but also on base normal-state map as `y`.
Region-based.
Can take a register modifier.
Note that unlike in vim, in this demo it does NOT take you out of visual state.
Because it's so often paired with delete or change.
Or at least I do.
Maybe I'll change that habit and actually make the habit of using registers.
Anyway, you can change that behavior if you want, as an exercise for the reader.

### transpose

`t` in command map.
Switch places with a neighboring object.
The cursor ends with on the original object, so you can continue to drag it with repeated transposition (eg. with the `.` repeat operator).
(Note that you can also give it a numeric argument for the number of times to transpose, but repeating can be visually easier.)

If there is an active region (visual-state), all instances of the object within the region are transposed.
† I haven't implemented region transposition yet, or decided exactly how it should handle cases where the region is not cleanly on top of the different objects.

### join

`j` in command map.
Join neighboring objects.
The most common use of this is probably joining lines.
But also join neigboring s-expressions, neighboring XML trees, etc.
† I think I've only implemented this for lines and smartparens s-expressions.

### split

`J` in command map.
Split an object at point.
Eg. for smartparens this means split the list containing point into two lists, at point.
Or for lines, insert a newline at point.
† I think I've only implemented this for lines and smaratparens s-expressions.

### slurp

`s` in command map.
Tree operation.
Turn the next sibling node into the last child node (for forward slurp).
🤷 I haven't been very strict about whether this only works on sibling nodes, or whether if there is no sibling node it will then slurp the parent's sibling to be the parent's last child, etc.  This is something that should be modulated by the `respect-tree` modifier.  Though I think I would prefer the default for slurp to be not to strictly respect the tree, and to do the flexible slurp.  But I think I've only implemented the strict slurp where I have implemented it, while smartparens does the flexible slurp.

Backward slurp turns the previous sibling into the first child.

### barf

`b` in command map.
Tree operation.
The opposite of slurp.
Turn the last child into the next sibling (for forward barf).
For backward barf, turn the first child into the previous sibling.
Note 🤷 caveat for `slurp`.

### open

`o` in command map.
Move cursor to an appropriate place for the object, maybe insert some boiler plate, and enter insert state.
For trees it opens a sibling node.
Eg. for open line forward, insert a new line after the current line (and auto-indent) and enter insert state.
For open outline forward, insert a new sibling heading after the current heading.

Pair with `down` modifier to open a child node (can add numeric modifier to open the Nth child), or with the `up` modifier to open a sibling of the parent (or other ancestor with numeric modifier).

### promote

`W` in command map.
Tree operation.
This is “unwrap” or “splice” for s-expressions, XML, or treesitter, it is promote-subtree for outline (IE reduce the number of bullets for all headings in the subtree), dedent for indent trees.
† I think this is just implemented for s-expressions and outline right now, maybe poorly for indent trees.

### demote

`w` in command map.
Tree operation.
This is “wrap” for s-expressions, and must be paired with the `delimiter` modifier to specify one, or asks for you to enter one.
† query for delimiter not implemented.
For XML, it must also have the `delimiter` modifier or query for a tag.
† nothing at all implemented for xml
For outline it is demote-subtree (increase bullet count), for indent trees it is increase indentation.
† otherwise same limitations as promote.

Note that if region is active, you can do `swl(` or `swl{`, etc, to wrap region with that delimiter.
The `l` is mostly vestigial in this example, since direction doesn't matter, but you need to enter the object selection map.


### change-delimiter

`hw` in command map.
Tree operation.
† Not yet implemented.  But I'm sure you can imagine what it is supposed to do for s-expressions, XML, or other places wrapped by delimiters.  This is pretty egregious as far as missing functionality goes, but I guess it's not built in to vim or evil-mode either.  But vim-surround and evil-surround are core add-ons to those that you obviously want.

### paste-to-region-from-move

`hp` in command map.
Also bound to `p`, with immediate execution rather than composition, on normal map.
So as `p` it just pastes at point, or replaces the region with a paste.
As `shp`, you compose it with a movement to make a region.
Takes the `register` modifier.
Also note the section on registers for configuration for default register to read and write to.

### move-paste

`p` in command map.
Does a move, then paste at the end of the move, but return your cursor.
Takes the `register` modifier.

### move-insert

`A` in command map.
Does a move, then enters insert state.
The rationale for this is that it is a generalization of the `A` and `I` keys in vim, which are useful not just because they are convenient to move to the appropriate place, but also because the movement is captured along with the entered text for repetition.
But maybe in cases where you might use move-insert, you would just record a keyboard macro instead.
But maybe it's also useful because if you are customizing your keymap with common movements, maybe you would add keys for eg. move-insert with some other objects and modifiers for convienent access and repetition.
`A` and `I` are great because `line` is arguably the king of text objects.
But maybe you would rather have easy access to inserting at the end of the current s-expression or definition or statement block or... whatever.

### upcase

`U` in command map.
Region-based.
Make the characters in the region uppercase.
Mostly useful to pair with `word` or `symbol` objects.
Or maybe you're a lawyer and you need to convert whole paragraphs to VERY IMPORTANT LEGALLY BINDING VERBAGE THAT YOU MUST READ.

### downcase

`u` in command map.
Region-based.
Make the characters in the region lowercase.

### toggle-case

`~` in command map.
Region-based.
Turn the uppers lower and the lowers upper.
Is this actually useful?
I'm not certain.

### capitalize

`hu` in command map.
Region-based.
Capitalize the first letter of each word.

### indent

`i` in command map.
Region-based.
Increase indentation for lines in the region.
† I think I haven't actually implemented this yet.

### dedent

`I` in command map.
Region-based.
Decrease indentation for lines in the region.
† I think I haven't actually implemented this yet.

### auto-indent

TODO - this is different from format, even though it is the most important 90% of format, and typically the thing that I do while editing, leaving formatting for something that I wouldn't usually compose.  But when making this I wrote format and not auto-indent.  Maybe they should both exist.  Maybe they are redundant.

### format

`f` in command map.
Region-based.
Auto-format the region.
† I think I haven't actually implemented this yet.

### comment

`hc` in command map.
Region-based.
Comment-out the region.

### uncomment

`hC` in command map.
Region-based.
Uncomment the region.

### toggle-comment

`C` in command map.
Region-based.
Comment or uncomment the region.

### initiate-isearch

`n` in command map.
Region-based.
This is a generalization of `*` and `#` from vim.
IE search for the thing at point, but where vim's `*` and `#` are specifically the word at point, this composes with object selection so that you can search for the word at point, or the symbol at point, or the line at point, whatever.
† Not yet implemented.



## Modifiers

Modifiers in this system are like adjectives and adverbs in spoken language.
They can be used in the composiphrase sentence matching system to choose which command to map to, and/or can be passed as arguments to the matched function.
As with adjectives and adverbs in spoken language, sometimes they make sense, and sometimes they don't.
I've tried to find useful and intuitive ways to compose them.
I'm sure there are other composed operations you might think of, and there are certainly other different encodings that may be better/worse/have tradeoffs.

In this demo, the modifiers are typically bound in the object map, but importantly when selected they leave you still in the object map.
So upon entering the object map, you can repeatedly select modifiers, then finish by choosing an object.

### number

You can write the number at any point before the object.
Typically the number indicates the number of moves for commands that operate on a region based on movement (like delete, change), or number of times to do something (for transpose, slurp), or some detail of the operation (eg. `2soldo` opens a new child heading at index 2).
Honestly, I don't use number modifiers very often, I more often repeat things.
See the section on repeating below.

### location-within: end

Bound to `n` in the object map.
Makes movement go to the end of an object, instead of the default beginning.

### location-within: beginning

This is the default.  Move to the beginning of things.  But you would maybe want to bind this if you changed that default.

### location-within: emacs-style

For typical built-in emacs motions, and extension packages that want to fit in well, motions move to the end of things when going forward, and to the beginning when going backward.
I didn't bind this in, but it would be eminently reasonable to make this be the default.  But then you might want to bind beginning and ending both somewhere to use at times.

### inner

`i` in object map.
Can combine with selection for trees to select the child region.  Eg. inside paretheses instead of selecting the parens as well, or inside xml tags, or indented lines in an indent tree without the parent line, etc.
Can combine with `up` tree movement to go to those same inside locations.
Also with `line` object, selects without final newline.

### tree-vertical: up

`u` in object map.
Can combine with movement to go up to parent.
The encoding here could be massaged a bit.
I use forward and backward plus up to go to the open/close paren, for example, but there is some overlap with begin/end modifiers.
When combined with selection, select the root-level tree.
When combined with transposition, reorder ancestor nodes.
When combined with open verb, eg. with outline, to open a new header at a higher level (useful when finishing a nested note and wanting to move out to higher level, and not enter all of the astserisks).
† Open-up should work with any kind of tree, but I think I've only implemented it for outline.

### tree-vertical: down

`d` in object map.
Can combine with movement to go down trees.
The encoding here could be massaged a bit.
There is some overlap with begin/end modifiers.
Can combine with open verb, eg. with outline, to open a new child.
† Open-down should work with any kind of tree, but I think I've only implemented it for outline.

### inorder

`T` in object map.
Make forward/backward motion for trees be an in-order traversal instead of moving to siblings!
Ok, it's not that useful.

### register

`r` in object map,  but also `"` in base map.
(Hmm, maybe that deserves more thought.)
After selecting this modifier, you must enter another key to be the register name.
The register is used for operations like copy, paste, delete, change.

### surrounding-space

Bound to space key in object map.
Makes movements and selections also take surrounding space.
† Not yet implemented.  Also, I haven't decided how I want to deal with leading space vs trailing space vs surrounding space.

### absolute

Bound to `b` key in object map.
Make movements mostly ignore `forward` and `backward`, and instead go to the Nth element.
`forward` and `backward` are still used if no number is given, to mean first or last.
Negative numbers are supported, going to the Nth from the end.
For trees, go to the Nth sibling, unless the `up` modifier is used.
† I think I've only implemented this for line and character.

### matching

Bound to `m` in object map.
Makes movements move to the next thing that matches the current one.
Eg. the next word that is the same as the word at point, or the next instance of the symbol at point, or the next line that is the same as point, or the next sexp that has identical contents (modulo white space, hopefully!), etc.
† I haven't implemented this.

### alternate: alternate

Bound to `a` in object map.
This is sort of a catch all, to provide alternate behaviors when there could be multiple.
† I have only used this one rarely, so far, though there are various ways I've thought about using it but haven't implemented.
Eg. switch between vi word and vi WORD, switch between symbol and symbol segment (splitting at underscores, dashes, camelCaseSubWords, or such).
These differences are object (or maybe verb) specific, and thus are hard to capture in a modifier that is really re-usable, without lumping a bunch of unrelated concepts together.

### alternate: alternate-2

Bound to `A` in object map.
Maybe there are multiple alternate ways of looking at things.

### respect-tree

The default for tree operations is to respect the tree.
Eg. don't move past the last sibling.
I prefer this as the default, others may not.
I bound this to `hR` in the object map, it looks like.

### respect-tree: nil (disrespect-tree)

Bound to `hr` in the object map.
Stop respecting tree boundaries!
This is particularly useful when paired with moving to a specific kind of node, or to a list with a specific delimiter, etc.
† I've hardly implemented this for anything.

### idempotent

This is intended for movements, making them not move if they are already at a place they would have moved to.
Eg. Vim's `$` goes to the end of the current line, which would be `forward idempotent end line`.
† I haven't implemented this at all.
I think this could be useful for other motions, too, though, for keyboard macros.
Sometimes you want to move to a particular anchor point, but may or may not already be there at the end of an operation.
So an idempotent movement lets you ensure that you are at said anchor point for the next step in the macro.

### current-line-only

This makes things only operate on the current line.
† I haven't implemented this at all.
The main uses of this would be... the same as idempotent, to go to the beginning or end of line.
Except maybe this is a better encoding that is useful in more placess.
Eg. back-to-indentation and move-to-start-of-trailing-space make sense if implemented with current-line-only instead of idempotent.



## Repeating

In this composiphrase demo config, there are several different ways to repeat different things.

- Repeatable motions -- most motions are repeatable, so after you do any of the motions by using the default `move` verb with an object, you can repeat that motion with the `f` key (or the `;` key), with the same numeric prefix (or you can give a numeric argument to `f`/`;` to switch the number).  Reverse the direction of the repeat  with `F` or `,`.  Why both bindings?  Because I use `f` and `F`, and left them in, even though a lot of this demo config doesn't actually match my config, but `;` and `,` are more relatable to the default vim config.  I should probably delete one or the other.
- Some repeatable motions (in the default config, only `j` and `k`) are repeatable but only if you explicitly give a numeric argument to the original movement.
- Search repetition.  Searching with `/` and `?` can be repeated with `n` and `N`, like in vim.  I find it useful to keep this separate from the other repeatable motion, largely because I don't want other motions to make moving to a search result go away.
- Edit repetition -- use the `.` key to repeat the last command (group) that edited the buffer.  I tried to make this similar to Vim's repetition.  IE it captures the building and execution of a command sentence, and if the command sentence leaves you in insert state, it captures all of that, too.  The repetition system is always recording the keys used for commands, and splits them when it can detect that it is back in normal state with an empty command sentence, basically.  🤷 Except I didn't think about recursive editing when I wrote this, so it is broken for commands that use recursive editing.  So if you do something like `sc/text<enter>` then use `.`, you will get a mess.
- Macro recording -- record keyboard macros to capture larger and more custom groupings!  See the normal state key map explanation below, as the keys for recording and playing macros differ from vim.


## How to Exit, how to save, etc

Since I've bound `:` in this system to use `evil-ex` from the evil-mode package (despite otherwise not using it), you can use `:q` if you are used to that from vim.
Or you can use `C-x C-c` like the Emacs folks do.
Or you can use `M-x` and use the `kill-emacs` command.

For saving, you can likewise use ex commands like `:w`, or use the emacs way, etc.
I use my own custom key binding for saving, and when I (occasionally, for weird reasons) use vanilla emacs, I just use `C-x C-c`, one of few emacs bindings not used in shells that I know, and then it prompts to save.
I don't know offhand what the normal emacs key binding to save is.

## Register handling for copy/yank, delete, change, paste

For copy/paste, this Composiphrase demo has the various commands use registers, like Vim.
Each command has a default register that it puts text into (or uses to paste from).
You can configure these.
Each of these also can use the `register` modifier in the command sentence.

Emacs also has a complicated copy/paste system that is different, called the kill ring.
If you use an operation that copies to the register set in `cpo-copy-sync-with-kill-ring-register`, it will also put that copied thing on the kill ring.

Depending on the emacs setup, it may or may not sync the kill ring to the system clipboard.

Personally, I use a separate key binding, which I haven't put in the demo config, for copying and pasting with the system clipboard.

In Vim, by default, pasting over a region, delete, and change, all copy the deleted text into the default paste register.
I find this annoying.
So in this demo config, by default delete, change, and saving from a paste, all have different default registers than copy and the read half of paste.

TODO - variable names for configuration.

## Cursor Positioning

Unlike Vi and most of its descendants, composiphrase always considers the cursor to be between characters, not [on characters](https://www.willghatch.net/blog/text-editing/cursor-positioning/).


## States

Composiphrase has normal state, like vim, for commands, except that we are composing larger sentences with composiphrase.
It has insert state.
Go from normal to insert state typically with `i`, but also with other commands.
See the full keymap.

Go from insert state to normal state with `C-c`, or with escape.

Go from normal state to visual state with `v`.

Go from visual state to normal state with `C-c`, or escape, or `C-g`.

Also there is a pager state, but I think I didn't bind anything in this demo yet.
Maybe I'll do that later, but I'm trying to finish this up, and scope has already creeped too much.


## Cancelling

If you have a sentence built up, but you changed your mind about it, use `C-g` (control+g) to cancel.
`C-g` is typically the cancel button in emacs.

## Undo / Redo

TODO - mention undo-tree, which is used for the demo
TODO - mention movement undo, and maybe also other ways to return to previous positions in emacs like pop-tag-mark

## Keys overview

### Normal and visual states

This is a snapshot of the current state of the demo.
Remember that the demo isn't stable, though you are free to fork it or otherwise copy it, or version pin it, or whatever.
And it's a “narrowly focused” demo to what Composiphrase is about.
If you were to really use it, you would want to do a bunch of other emacs configuration and bind other things, too.

- `a` is like Vim's `a` in visual state, except it is also selection in normal state.  The cursor is always between characters, so you don't have the one-off difference between `insert` and `append`.  Concretely, it sets the `direction` modifier to be `expand-region` (maybe not a great encoding), and enters the object map.
- `A` is left alone in this demo -- it does what it does in Vim.  It is redundant with `sAlnl`.  But I'm considering mapping it to be an alias for `sAl`, leaving the specific object (and modifiers) to move by out.
- `b` unbound.
- `B` unbound.
- `c` like in vim.  Redundant with `sc`.  See the section on registers.
- `C` like in vim.  Redundant with `scnl`.
- `d` like in vim.  Redundant with `sd`.  See the section on registers.
- `D` like in vim.  Redundant with `sdnl`.
- `e` unbound.
- `E` unbound.
- `f` Repeat motion (in same direction as the original motion).  The vim behavior, (well, something similar but slightly different), is bound to `ef`.  Note that vim uses f/t to go up to or on the character, while composiphrase uses the beginning/end position modifiers (or, well, default to beginning and give end as an explicit modifier, typically).
- `F` Repeat motion (reverse direction).
- `g` is a prefix in vim, and is here, too, but it really just has `gg` bound to work like in vim -- go to line, defaulting to the first line.  Maybe I should make it a “goto” map, like in Helix.  But this demo is already big enough for now.  Also `gg` is redundant with `absolute` modifier `b` with line.  Use `5hbl` to go to line 5.
- `G` like in vim.  Redundant with `lbl`.
- `h` sets the `direction` modifier to be `backward` and enters the object map.  Move a single character by `hc`.  But don't move by single characters unless you are actually in a one-off situation.
- `H` unbound.
- `i` is still insert in normal state.  In visual state, or when the current command sentence is not empty, it is instead an alias for `ai` (select inner).
- `I` like in vim.  Redundant with `sAhal`.  But I'm considering mapping it to be an alias for `sAh`, leaving the specific object (and modifiers) to move by out.
- `j` and `k` are still down/up line, keeping the same column.  Or, they are effectively just `next-line`/`previous-line` from emacs, but with visual line motion always disabled.  I find that I like Vim's column tracking in this case better, but so far I'm living with the emacs version.  Note that these are not the same as `hl`/`ll`, which both move specifically to the beginning of a line, or `hnl`/`lnl` which specifically move to the end of a line, consistent with explicit beginning/end motions for other objects.  Note that `j` and `k` are not bound to the normal repeatable motion variants, but are bound to the `rmo-c`, repeatable-motion-count variant, so `f` won't repeat moving by j or k unless you also use a number with j/k.
- `J` unbound.  You could bind it like in vim, but I'm leaving that as an exercise to the reader.
- `K` unbound.
- `l` sets the `direction` modifier to be `forward` and enters the object map.  Move a single character by `lc`.
- `L` unbound.
- `m` saves cursor location to a register.  You have to give a register name (character) as the next key.  Similar but not exactly the same as vim, since vim has separate marks and registers.
- `M` unbound.
- `n` like in vim, repetition for searching, which I've left separate from other motion repetition.
- `N` like in vim, repeat search backward.
- `o` unbound.  It could be bound like in vim, but I'll leave that as an exercise for the reader.
- `O` unbound.  It could be bound like in vim, but I'll leave that as an exercise for the reader.
- `p` paste.  In visual mode it replaces the active region.  Since the cursor is always between characters, there is no before/after distinction like in vim.  See the section on registers.  Also note the `move-paste` verb, which is different, which is bound to `sp`, and pastes after a movement, but leaves your cursor in the same place.
- `P` unbound.
- `q` Multi-purpose keyboard macro key.  When not recording, start recording keyboard macro to default macro register.  Note that the vim version asks for a register, where as this one just starts.  When already recording a macro, stop recording.
- `Q` Start recording a keyboard macro, but it prompts for a key to specify a register to record to.  Note that if you specify registers with `Q`, you can do recursive keyboard macro recording, where you record one macro in the middle of another.  Is that useful?  Maybe.
- `r` Replay the most recently recorded keyboard macro.  Maybe I should change this to be the macro in the default macro register?
- `R` Replay a macro from a given register (which you type as the next key).
- `s` Command map prefix key.
- `S` unbound.  You could bind this like vim, exercise for the reader.
- `t` unbound.  See note about `f`.
- `T` unbound.  See note about `f`.
- `u` undo.  Note that redo is bound to `C-r`, like in Vim.
- `U` unbound.  I never use vim's undo-line functionality.  I think evil-mode implemented it, but I always use regular undo.  I'm not going to implement vim's functionality for this.
- `v` enter visual state.
- `V` unbound.  I haven't implemented visual line mode.  See [this blog post](http://www.willghatch.net/blog/text-editing/dethroning-the-king-of-text-objects_visual-line-mode/).  Also note that I haven't implemented visual block mode (`C-v` in vim).  Emacs has a `rectangle-mark-mode` that has some functionality overlap.  Evil-mode actually implements something nicer than Vim's visual block mode, because `I` and `A` work more intuitively in it, and I miss that, but I will probably not implement it as a state, exactly, and it will be outside the scope of this demo, although I will almost certainly write something to insert/append to every line in the rectangle at the beginning or end of the rectangle...
- `w` unbound.
- `W` unbound.
- `x` delete character forward, like in vim.
- `X` delete character backward, like in vim.
- `y` copy, or “yank” in vim parlance, but emacs uses the work “yank” to mean paste, so let's call it copy.  If in visual state, copy the region.  Unlike vim, don't leave visual state.  If not in visual state, take a movement/selection to make a copy.  Redundant with `sy`.  See the section on registers.
- `Y` copy current line, like in vim.  But note that pasting a line is not as convenient as in vim, since it just copies the text and not some extra metadata saying that it was a line copy and needs special handling.  But you can paste it nicely with the `move-paste` verb, which is bound to `sp`.
- `z` bound to `execute-extended-command` or `M-x`.  This was unnecessary, but I did this in my personal config and I decided to leave it in this one.  It is nice to have a single-key entrance to this instead of `M-x` in normal state.
- `Z` unbound.
- `:` bound to evil-mode's ex implementation.  This is one piece of evil-mode that I want to keep and not rewrite, so I'm leaving it in this demo.
- `;` bound to repeat motion, same as `f`.  Note that this doesn't just repeat finding charcters, but any repeatable motion (that's not search).
- `,` like `F`, repeat motion backward.
- `.` repeat the last command that edited the buffer.  Or more concretely, this is basically a keyboard macro that repeats all keys between certain events, so that all of the keys that make a command sentence, plus any writing in insert mode, are repeated.  But... Look, my implementation of this is currently broken, I didn't think about recursive editing when I wrote it, so it doesn't work with things like searching that cause recursive editing, the keys to repeat become garbled. 🤷
- `<`, `>` unbound, exercise for the reader, but the verb map has indentation.
- `/` and `?` like in vim, start incremental search.  Note that after searching, I have bound enter to go to the start of the match and `C-j` to go to the end of the match, and this behavior is repeated when using `n`/`N`.
- `'` like vim, ask for a register name, and go to that point.  Except vim keeps registers and marks separate, and I haven't.  Also, out of laziness, and because I only ever use the backtick version, I made both go to the mark instead of the start of the line of the mark.
- backtick (I'm not sure how to format a backtick in markdown, and don't want to look it up right now) like in vim, go to mark, except it is a register.
- `"` like in vim, it sets a register modifier.  This is also in the object map as `r`.
- `|` unbound.  Use `absolute` modifier with character to do this.  Eg. `10hbc`.  Note that `alternate` `absolute` character motion is buffer position, `10habc` goes to character 10 of the buffer instead of character 10 of the line.  Also note that columns in emacs are zero indexed, while vim uses 1 index, while lines in emacs are 1 indexed like every editor.
- `-` negative modifier for numbers.
- numbers: like in vim, except 0 is just a number.  Sometimes commands in emacs can actually take a 0.  Instead of what `0` does in vim, try `hl`, although that isn't quite the same.  Add the `idempotent` modifier to actually be like `0`, except I haven't implemented that yet...  🤷
- `!` unbound, exercise for the reader.
- `@` unbound, use `R`, or rebind as exercise for reader.
- `#` and `*` unbound.  Something similar is bound to `sn`, except I haven't implemented it yet.  🤷
- `$` and `^` unbound, exercise for reader.  See `idempotent` and `same-line` modifiers.
- `&` unbound, though I should add this.
- `(` and `)` are bound to select smartparens sexp, except only one that has a delimiter, but any delimiter instead of a specific delimiter, wich `(` bound to normal selection and `)` bound to also add the `inner` modifier.  This is a convenient binding that I use all the time, but it's not really very compositional, is it.  Note that the object map has sp-sexp with specific delimiter bound to parentheses, braces, brackets, etc.
- Any other symbol is probably unbound.  More exercises for the reader.

Note that operators in Vim that let you double tap them to operate on the current line don't have that behavior in composiphrase, or at least this demo configuration of it.

TODO - movement undo.
TODO - I know there was something else that I wanted to discuss here, but what was it?
TODO - put a graphic of the key maps somewhere, maybe.

### insert state

Insert state mostly defers to the Emacs global key map.
So you have emacs keys in insert state.
This is nice, since it matches the default bindings in eg. shells and such.
You probably know some of these bindings.

But, we need to get back to normal state.
So `C-c` and escape are both bound to returning to normal state.

### visual state

Mostly the same as normal state, but `i` is always an alias for `ai` and never inserts.
Go back to normal state with `C-c` or escape or `C-g`.

### Demo emphasis

I want to re-emphasize that you should view these key bindings as a demo, or a particular configuration.
I hate that programs ship with typically poorly thought out key bindings, and those binding immediately ossify and can never be changed.
And even if they are well thought out, things change, and you want to change them.
The first thing I do when configuring a new emacs package is typically try to find the variable names of its keymaps and then wipe them.
I wish setting a keymap for a given package were something that you did explicitly -- yes opt in to the default key bindings, or this alternate key binding set, or do none at all.
This would allow configurations to be future proof while allowing the defaults to change over time.
Eg. emacs could expect everybody to have (use-emacs-default-keybindings-2025) as the first line of their configuration (or other years, or have an argument, etc), and then actually be able to evolve the default key bindings, or provide several different defaults.
Just think, when is the last time you hit `C-p` in a typicaly application and actually wanted to get the print dialog?
This is a lesser axe to grind than that all programs should actually have configurable key bindings (and have them configurable in a way that you can reasonably save the configuration in eg. a git repo and easily use it accross all of your computers), but still.

## The libraries

This demo is composed of multiple of my packages, most of which I wrote or published more-or-less in service of this demo, but that provided orthogonal functionality that I wanted to split up:

- [repeatable-motion](https://github.com/willghatch/emacs-repeatable-motion) provides, well, repeatable motions
- [estate](https://github.com/willghatch/emacs-estate) provides stateful/modal editing
- [composiphrase](https://github.com/willghatch/emacs-composiphrase) the library is actually just a small library that provides the matching system for composing and executing sentences.  I may move the demo matching config into that package at some point to be the default matching config.
- [composiphrase-objects](https://github.com/willghatch/emacs-composiphrase-objects) provides... basically all of the object implementations.  So I published several micro packages and also a big package of loosely related things, where the relation is that they are trying to fill out the table of operations for each of the objects.  Most these are wrappers, I think I only implemented the indentation tree object from scratch.  But they all provide some interesting new functionality to the thing that they wrap.

There are some things left in the demo at the moment that could be split out or moved to those other packages, but they are typically in a slapdash state.
I mean even more so than some of the slapdash in those packages mentioned above.
For example, the library for recording and replaying commands behind the `.` repeat command that doesn't work with recursive editing...

Also this demo relies on:

- [smartparens](https://github.com/Fuco1/smartparens), for its excellent and configurable (though... also slower than some alternatives) implementations of many tree operations.
- built-in packages like treesit, outline-minor-mode, org-mode, and tons of core features.

This demo also includes a mode line configuration that is basically a copy of my personal mode line configuration, and a theme that is a copy of my personal theme.
I haven't used the default mode line since approximately the first day I used emacs, and I didn't want to bother figuring out how to display the estate state or composiphrase sentence state in the default mode line.
And since my mode line looks terrible without my theme...
Well there you go.
You're welcome or I'm sorry, depending on how you feel about my visual aesthetic choices.

## Install and run the demo

### Install

- install emacs, however you get it.  The Treesitter components need Emacs version 29, but probably everything else works with version 28, or even several versions lower probably.
- clone this repository: `git clone https://github.com/willghatch/emacs-composiphrase-demo`
- Enter the directory: `cd emacs-composiphrase-demo`
- Read the install script: `cat install.el`
  It installs some emacs packages in a sub directory.
  I could instead have the init file install things on first run, but then startup is slow always.
  I like a fast emacs startup.
  Even if I do use the server and client setup sometimes to make it even faster.
- Run the install script: `emacs -q -batch -l install.el`

### Run

- `emacs -q -l $COMPOSIPHRASE_DEMO_DIR/demo-init.el $FILE_TO_EDIT`
- Maybe put that in an alias definition, and try using it for a day for your text editing.

## The end

This is basically as narrow of a demo as seemed good to present a competitor to vim.
In a full configuration obviously you want to pull in a bunch of other emacs features, bind other commands, configure things, etc.
But this demonstrates a reasonably feature complete *text editor*, which is the thing that Emacs ostensibly lacks.

