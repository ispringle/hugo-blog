+++
title = "Still Undecided"
author = ["Ian S. Pringle"]
date = 2022-08-27T19:54:00-05:00
tags = ["meta", "blog"]
draft = false
+++

Me, the jury of one, is still out on whether I will continue with the
Uniorg+NextJS or attempt to find a different solution. The current trouble?
Parsing of Orgmode verse blocks results in a `<pre>` , which is not at all what I
want. I could make most of the verse blocks, most of the time, look good. But
what if it contains superscript text? In Org that looks like a `^{I am
superscript}` or a `^superscript`. But when that translates to HTML it ends up
being wrapped as `<sup>I am superscript</sup>`  which doesn't work with the `<pre>`
block since these blocks render text verbatim. This is a problem for Bible
verses which have whitespace formatting (which is that the Orgmode verse block
is meant to preserve) but also have superscript text for the verse numbers. Take
a look at Isaiah 8:13:

<div class="verse">

﻿<sup>13</sup> It is Yahweh of hosts whom you should regard as holy.<br />
&nbsp;&nbsp;&nbsp;&nbsp;And He shall be your fear,<br />
&nbsp;&nbsp;&nbsp;&nbsp;And He shall be your cause of trembling.<br />

</div>

This is extra tricky, not only is there some whitespace formatting, but the
superscript is the very first element in the text which, in Orgmode, requires
that you use a special zero-width, non-breaking space character.

What to do? I don't know. I was just getting happy with the idea of continuing
with NextJS+Uniorg too. Suffice to say, I am going to continue plodding away at
my [build](../build) system for org-publish, because the exported Orgmode HTML does a
wonderful job of properly displaying this exact situation:

```html
<p class="verse">
﻿<sup>13</sup> It is Yahweh of hosts whom you should regard as holy.<br>
&nbsp;&nbsp;&nbsp;&nbsp;And He shall be your fear,<br>
&nbsp;&nbsp;&nbsp;&nbsp;And He shall be your cause of trembling.<br>
﻿<sup>14</sup> Then He shall become a sanctuary;<br>
&nbsp;&nbsp;&nbsp;&nbsp;But to both the houses of Israel, a stone to strike and a rock to stumble over,<br>
&nbsp;&nbsp;&nbsp;&nbsp;And a snare and a trap for the inhabitants of Jerusalem.<br>
﻿<sup>15</sup> And many will stumble over them;<br>
&nbsp;&nbsp;&nbsp;&nbsp;Then they will fall and be broken;<br>
&nbsp;&nbsp;&nbsp;&nbsp;They will even be snared and caught.”<br>
</p>
```
