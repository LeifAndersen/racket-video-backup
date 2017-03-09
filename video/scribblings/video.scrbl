#lang scribble/manual

@;{
   Copyright 2016-2017 Leif Andersen

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
}

@require[scribble/core
         scribble/example
         racket/sandbox
         (except-in pict table)
         video/private/utils
         video/private/surface
         @for-label[(except-in racket/base filter)
                    racket/contract/base
                    racket/set
                    racket/hash
                    (except-in racket/class field)
                    racket/gui/base
                    racket/draw
                    video/base
                    video/core
                    video/render
                    video/init]]

@title{The Video Language Guide}
@author{Leif Andersen}

@(defmodulelang video)

@(define (colorize #:color c . content)
   (elem #:style (style #f (list (color-property c)))
         content))

@colorize[#:color "red"]{
  This is a highly unstable and experimental DSL for editing videos. Do
  not use this library in production as core parts of it are still being
  designed.}

Video Language is a DSL for
editing...videos.@margin-note["Creative name, I know..."]
Designed with similar designs in mind to Scribble,
Slideshow, and Pict. It is still a work in progress and the
interface may or may not change in the near future.

@(compound-paragraph
  (style #f '())
  (list
   @para{This libraries requires that you have the following
 libraries on your system.}
   @itemlist[
 @item[@hyperlink["https://mltframework.org/"]{libmlt}]
 @item[@hyperlink["https://ffmpeg.org/"]{ffmpeg/libav}]
 @item[@hyperlink["http://ftp.gnome.org/pub/GNOME/sources/gdk-pixbuf/"]{GDK Pixbuf}]
 @item[@hyperlink["https://frei0r.dyne.org/"]{Frei0r}]
 @item[@hyperlink["https://packages.debian.org/search?keywords=ladspa-sdk"]{ladspa-sdk}]]
   @para{Eventually Video will take care if
 this for you, but it currently does not.}))

@margin-note{Many of the implementation decisions of this
 library are based on my understanding of
 @hyperlink["https://mltframework.org/"]{libmlt}, and even
 requires the library to be installed on your machine to
 work. Unfortunately, libmlt lacks comprehensive
 documentation. If you see any invalid uses of libmlt, please
 notify me or submit a patch.}
