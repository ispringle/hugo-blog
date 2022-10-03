+++
title = "Example Org File"
author = ["Ian S. Pringle"]
date = 2022-09-23T00:00:00-05:00
tags = ["meta", "blog"]
draft = true
+++

## Introduction {#introduction}

This page shows some examples. The org source document is [here](https://raw.githubusercontent.com/ptpt/org-notes-style/master/examples.org).


## VERSE <span class="tag"><span class="orgmode">orgmode</span></span> {#verse}

VERSE is used to keep the line breaks in a reagin. This is a VERSE:

<div class="verse">

Great clouds overhead<br />
&nbsp;Tiny black birds rise and fall<br />
&nbsp;&nbsp;Snow covers Emacs<br />
<br />
&nbsp;&nbsp;-- AlexSchroeder<br />

</div>

Notice: As you can see, the starting whitespaces counts.


## This is a quote <span class="tag"><span class="orgmode">orgmode</span><span class="example">example</span></span> {#this-is-a-quote}

This is a long quote:

> Let us change our traditional attitude to the construction of programs: Instead of imagining that our main task is to instruct a computer what to do, let us concentrate rather on explaining to human beings what we want a computer to do.
>
> The practitioner of literate programming can be regarded as an essayist, whose main concern is with exposition and excellence of style. Such an author, with thesaurus in hand, chooses the names of variables carefully and explains what each variable means. He or she strives for a program that is comprehensible because its concepts have been introduced in an order that is best for human understanding, using a mixture of formal and informal methods that reinforce each other.
>
> — Donald Knuth

This is a short quote:

> Everything should be made as simple as possible, but not any simpler
> -- Albert Einstein

Just like a normal paragraph but it is quoted.


## QUOTE Quoted level {#quote-quoted-level}

This level is quoted


## `EXAMPLE` {#example}

An EXAMPLE is used to include literal examples.

This is an EXAMPLE:

```text
  Some example from a text file.
```

You can also start the example lines with a **colon** followed by **a
space**:

```text
Some example started with a colon followed by a space.
```

If example is source code, you can use `BEGIN_SRC` block instead.


## Source code {#source-code}

You can always use `M-x org-edit-src-code` to edit the code block in
another buffer.

Print `hello world` in C:

```c
    #include <stdio.h>

    int main(int argc, char *argv[])
    {
         printf("hello this world\n");
         return 0;
    }
```


## Markups {#markups}

-   _This is italic_
-   **This is bold**
-   This is <span class="underline">underlined</span>
-   This is a `variable`, `verbatim`
-   ~~this is deleted~~

This is a horizontal line

---


## Drawers {#drawers}

This is a drawer


## Org Footnotes {#org-footnotes}

The Org homepage[^fn:1] now looks a lot better than it
used to. Inline definition&nbsp;[^fn:2]. An issue about the footnote&nbsp;[^fn:3].


## ox-hugo shortcodes {#ox-hugo-shortcodes}

Using ox-hugo means we can do some cool stuff
{{% sidenote %}}
like injecting Hugo shortcodes with org blocks
{{% /sidenote %}} .


## Links {#links}

This is a [link](http://www.google.com) do you like it?


## Lists {#lists}

-   plus
-   second plus
    -   star
    -   second star
        1.  number
        2.  second number
            1.  number again
            2.  second number again
    -   third star
-   third plus


## Math formula {#math-formula}

The formula:

\\[ f(x) = x^2 + 2 \\]

where \\(x\\) is some value and \\(f(x)\\) returns the result.


## <span class="org-todo todo TODO">TODO</span> Buy a car {#buy-a-car}

It's just an example :)


## <span class="org-todo done DONE">DONE</span> Upload this file {#upload-this-file}

-   add more examples
-   [ ] push to GitHub


## Top level headline {#top-level-headline}

The quick brown fox jumps over the lazy dog. Foxy parsons quiz and
cajole the lovably dim wiki-girl. Watch “Jeopardy!”, Alex Trebek’s
fun TV quiz game. How razorback-jumping frogs can level six piqued
gymnasts! All questions asked by five watched experts — amaze the
judge.


## Two stars {#two-stars}

The quick brown fox jumps over the lazy dog. Foxy parsons quiz and
cajole the lovably dim wiki-girl. Watch “Jeopardy!”, Alex Trebek’s
fun TV quiz game. How razorback-jumping frogs can level six piqued
gymnasts! All questions asked by five watched experts — amaze the
judge.


## Three stars {#three-stars}

The quick brown fox jumps over the lazy dog. Foxy parsons quiz
and cajole the lovably dim wiki-girl. Watch “Jeopardy!”, Alex
Trebek’s fun TV quiz game. How razorback-jumping frogs can level
six piqued gymnasts! All questions asked by five watched experts
— amaze the judge.


### Four stars {#four-stars}

The quick brown fox jumps over the lazy dog. Foxy parsons quiz
and cajole the lovably dim wiki-girl. Watch “Jeopardy!”, Alex
Trebek’s fun TV quiz game. How razorback-jumping frogs can level
six piqued gymnasts! All questions asked by five watched experts
— amaze the judge.


#### Five stars {#five-stars}

The quick brown fox jumps over the lazy dog. Foxy parsons quiz
and cajole the lovably dim wiki-girl. Watch “Jeopardy!”, Alex
Trebek’s fun TV quiz game. How razorback-jumping frogs can
level six piqued gymnasts! All questions asked by five watched
experts — amaze the judge.


## Table {#table}

<span class="org-target" id="org-target--sec-table"></span>

<a id="table--tab:plan"></a>
<div class="table-caption">
  <span class="table-number"><a href="#table--tab:plan">Table 1</a>:</span>
  This is the plan for next weekdays
</div>

| Monday | Tuesday                           | Wesday | Thursday | Friday | This is a long weekend                                            |
|--------|-----------------------------------|--------|----------|--------|-------------------------------------------------------------------|
| work   | play                              | study  | work     | party  | I have nothing to do                                              |
| play   | study and work and play and party | work   | party    | work   | I sing and dance and the quick brown fox jumps over the lazy dog. |


## Image {#image}

<span class="org-target" id="org-target--sec-image"></span>

<a id="figure--fig:tree"></a>

{{< figure src="https://c1.staticflickr.com/5/4105/5080934352_5e8e76b7f4_b.jpg" caption="<span class=\"figure-number\">Figure 1: </span>This is a tree taken by Tao. Source: <https://flic.kr/p/8JZ7Lq>" >}}


## Internal links {#internal-links}

-   one item
-   <span class="org-target" id="org-target--target"></span>another item

    Here we refer to item [2](#org-target--target).

    Please check the table [1](#table--tab:plan) at section , and image
    [1](#figure--fig:tree) at section .


## Special symbols {#special-symbols}

The mass of the sun is M_sun = 1.989 x 10^30 kg.  The radius of the
sun is R<sub>sun</sub> = 6.96 x 10^8 m.

Angles are written as Greek letters &alpha;, &beta; and &gamma;.

[^fn:1]: org mode homepage
[^fn:2]: This is the inline definition of this
    footnote
[^fn:3]: The definition is wrapped in
    a paragraph (`<p>`) hence a new line
