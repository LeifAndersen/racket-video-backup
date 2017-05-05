#lang scribble/manual

@require[scribble/core
         scribble/example
         racket/sandbox
         (except-in pict table)
         video/init
         video/private/surface
         @for-label[(except-in racket/base filter)
                    racket/contract/base
                    racket/set
                    racket/hash
                    (except-in racket/class field)
                    racket/gui/base
                    racket/draw
                    video/init
                    video/render
                    video/init]]

@title{The Video Language Guide}
@author{Leif Andersen}

