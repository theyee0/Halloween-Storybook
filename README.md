# Halloween-Storybook
An unconventional programming language with a syntax resembling a poorly written Halloween story.

# Demo
Here is an example of the process, starting from a source file implementing the game of guessing a number. It shows the compilation and execution process.

https://github.com/user-attachments/assets/bd14b107-b288-4415-b551-ce071a5cb059

# Getting started
To build the transpiler, you can run
```
sbcl --script build.lisp
```
to create a `halloween-storybook.exe` file corresponding to your system.

Then, you can compile files by running
```
./halloween-storybook.exe [INPUT FILENAME] [OUTPUT FILENAME]
```

## The language
This language does not exist outside of this project. It has mostly familiar semantics, but with some weird grammatical rules due to the nature of the project.
**You should look through the examples folder if you want to see some (relatively simple) examples of code**

# Syntax and Features
This language implements a subset of the functions from common lisp, but in a distinctly procedural style to match the goal of having a storytelling vibe.

## Abstractions
| Concept    | Translation  |
| ---        | ---          |
| Variables  | Graveyards   |
| nil        | Nobody       |
| True       | The user     |
| Integers   | Zombies      |
| Floats     | Ghosts       |
| Characters | Whispers     |
| String     | Curse        |
| Functions  | fable        |
| Macros     | Storybook    |
| Lists      | Ghost Trains |
| Maps       | Crypts       |
| Arrays     | Mausoleums   |

## Functions
| Operation  | Translation                                                                      |
| ---        | ---                                                                              |
| =          | "..." had the same feeling as "..."                                              |
| /=         | "..." was completely different from "..."                                        |
| <          | "..." was lesser than "..."                                                      |
| <=         | "..." was no better than "..."                                                   |
| >          | "..." was greater than "..."                                                     |
| >=         | "..." was no less than "..."                                                     |
| +          | "..." merged with "..."                                                          |
| -          | "..." had from it exorcised "..."                                                |
| *          | "..." was conflagrated by "..."                                                  |
| /          | "..." was split into groups of "..."                                             |
| mod        | "..." were left as a group of those who couldn't make a group of "..."           |
| defvar     | observe the faraway graveyard "..."                                              |
| 1+         | a new ghoul joined "..."                                                         |
| 1-         | a ghoul left from "..."                                                          |
| setf       | Invade "..." with "..."                                                          |
| let*       | Let me describe ["...", which is "..."]*                                         |
| abs        | the ghouls, spirit world or not in "..."                                         |
| random     | a blind apparition of at most "..."                                              |
| max        | the largest group in "..."                                                       |
| min        | the smallest group in "..."                                                      |
| when       | pretending that "..." then "..."                                                     |
| if         | if only "..." then "...", but in reality "..."                                       |
| and        | "..." in addition to "..."                                                       |
| string     | the chant "..."                                                                  |
| or         | "..." but alternatively that "..."                                               |
| not        | it is untrue that "..."                                                          |
| defun      | Let me tell you a fable called "..." about "..." but also "..."... {fn body} the end. |
| format     | greet "..." with "..." about "..."                                               |
| read-line  | the trick or treat cry                                                           |

## Notes
- All expressions will be interpreted in infix notation
- There are no parentheses (if you need this, you can assign intermediate results to a new variable)
- All lines must be terminated by a period
- Extra whitespace will be ignored
