# EAF
#### Parsing scanned bitmaps for flow diagrams and visual programming

# Basics

So here's approximately what we want

#### Example Input

    ..............................
    ....xx........................
    ...xxxxxxxxxxxxxxxxxxxx.......
    .xxxxxxxxxxxxxxxxxxxxxxx......
    ..xxxxxx....xxxx...xxxxxx.....
    ...xxxxx...........xxxxx......
    ...xxxx.............xxxx......
    ....xxx.............xxx.......
    ...xxxx............xxxx.......
    ...xxx.............xxxx.......
    ..xxxxx............xxxxx......
    ..xxxxx............xxxxx......
    ..xxxxxx.........xxxxxxx......
    ...xxxxxxxxxxxxxxxxxxxx.......
    ...xxxxxxxxxxxxxxxxxx.........
    ..........xxxxxx..............
    ...........xxxxx..............
    ...........xxxx...............
    ...........xxxx...............
    ..........xxxxx...............
    ..........xxxx................
    ....xxx....xxxxx....xxx.......
    ....xxxx..xxxxx....xxxxx......
    .....xxx...xxxx...xxxxxx......
    ......xxx.xxxxx..xxxxxx.......
    .......xxxxxxxx.xxxxx.........
    ........xxxxxxxxxxx...........
    ..........xxxxxxxx............
    ...........xxxxx..............
    ............xx................
    .........xxxxxxxxx............
    .....xxxxxxxxxxxxxxxx.........
    ....xxxxxxx....xxxxxxx........
    ...xxxxx..........xxxx........
    ...xxxx............xxxx.......
    ..xxxx.............xxxxx......
    ..xxxx..............xxxxx.....
    ..xxxx..............xxxxx.....
    ..xxxx..............xxxx......
    ...xxxx...........xxxxx.......
    ...xxxxxxx.....xxxxxxxx.......
    ....xxxxxxxxxxxxxxxxx.........
    .......xxxxxxxxxxx............
    ..........xxxx................
    ..............................
    ..............................
    ..............................

#### Acceptable outputs

Actual shape recognition _(I'm assuming hard)_

	Square (3, 4) (14, 21)
	Arrow (14, 13) (30, 13)
	Circle (37,12) 10

Line recognition _(maybe easier? but we can use it to generate the above within a certain tolerance)_

	Line (3, 4) (3, 20)
	Line (3, 4) (14, 4)
	Line (14, 4) (14, 21)
	Line (3, 20) (14, 21)
	Line (14, 13) (30, 13)
	Line (22, 3) (30, 13)
	Line (30, 13) (22, 22)
	Line (31, 12) (35, 4)
	Line (35, 4) (41, 4)
	Line (41, 4) (44, 11)
	Line (44, 11) (40, 22)
	Line (40, 20) (33, 21)
	Line (33, 21) (30, 13)

The circle might be composed of curves rather than straight lines. The output data format doesn't particularly matter; I'm only incidentally using the standard Haskell `show` format above because this happens to be a Haskell project.

### Direction mapping

This is the principal approach I've settled on for generating line vectors from bitmap line drawings. The relevant files are in [`Direction.hs`](https://github.com/Inaimathi/EAF/blob/master/Direction.hs). The main function defined is `getDirections`, which takes a sparse image and returns its cardinal and ordinal direction maps.

       ||                          +    \/                         
      --------------------         +   \o\////\\\\///\\\\o/        
    -----------------------        + \\o\o///oo\\oo//o\\\oo/       
     --|||-    ----   -|||--       +  \\o///    oooo   \\\o//      
      o|||-           -|||-        +   \o///           o\\o/       
      ||||             |||o        +   /\//             o\/o       
       |||             |||         +    /oo             //\        
      ||||            ||||         +   \//\            o///        
      |||             ||||         +   o\/             /o//        
     -|||o            ||||-        +  /o\\/            /////       
     -|||o            ||||-        +  /o\\\            ///oo       
     -|||--         --o|||-        +  /\\\\\         /////o\       
      --------------------         +   /o\\\\\\\\o////////\        
      ---------||-------           +   /o\\\\\\\\o///////          
             -||||-                +          \\\///               
              ||||-                +           \\///               
              ||||                 +           /\/o                
              ||||                 +           //\\                
             -||||                 +          ////\                
             -|||                  +          /o/o                 
       -o|    ||||-    |||         +    \\o    o\///    ///        
       --oo  -||||    -o---        +    \\\o  \\///    /////       
        o||   ||||   ------        +     \\\   oo/o   /////\       
         ||o -||||  ------         +      \\\ //o\\  /////\        
          ----|||o ----o           +       \\\//o\\ /////          
           ---|||-----             +        /\\/\\\////            
             -|||o---              +          \\o\////             
              ||||-                +           \\///               
               ||                  +            o\                 
            ---||----              +         ////\\\\\             
        -------||-------           +     o//////oo\\\\\\/          
       ||----     -----||          +    o/////     \\\\\\/         
      |||--          -|||          +   o////          \\\\         
      |||o            ||||         +   ////            \\\\        
     o|||             o|||-        +  \///             \\\\/       
     o|||              |||--       +  \\/o              \\o\o      
     o|||              |||--       +  //\o              o/\\\      
     o|||              |||o        +  /\\\              ///\       
      |||-           --|||         +   \\\\           /////        
      |||----     -----||-         +   /\\\\\\     ///////\        
       -----------------           +    /\\\\\\\/////////          
          -----------              +       \\\\\o/////             
             ----                  +          \\o/                 
                                   +                               
                                   +                               
                                   +                               
                                   +                               

Using this information, we can separate contiguous regions into islands, and assemble the dominating ones (by size) into a series of lines that represent the same image. That's done in the [`Elements.hs` file](https://github.com/Inaimathi/EAF/blob/master/Elements.hs), and the result of this operation is a series of `Element`s (just `Line`s at the moment, but `Ellipse` and/or `Curve`, and probably `Text` are coming in the future). Once we've generated elements, we align their points within a certain tolerance; things that _almost_ line up are made to line up precisely, on the assumption that our input is coming from humans.

The current output of this process can be seen in the [`test-data/` folder](https://github.com/Inaimathi/EAF/tree/master/test-data). Each `.txt`/`.ppm`/`.pgm` file is an input, and the corresponding output is in the similarly named `.svg` file. The current output for the above example input can be found [here](https://github.com/Inaimathi/EAF/blob/master/test-data/single-color.svg).

##### Still TODO

**Re-think the sorting criteria**

Right now, it's the length of the hypothetical line made from the region. That's probably good enough for ordinal regions, but we might actually want to grade cardinal regions by longest contiguous line rather than the naive bounding box (this would prevent the vertical line artifacts in diamonds and triangles observed in sanitized-input.* testing). We shouldn' *thin* cardinal regions this way, merely use it for ranking purposes, otherwise we get a really strict straightness requirement that basically means we need a ruler and stable surface out when drawing this stuff (at that stage, what's the point?)

**Tune up the alignment routine**

We currently align items with similar `x` and `y` coords globally. This means that we need to keep the alignment threshold relatively low in order to avoid radical changes, which results in some points that should be aligned being left separate. What we probably _should_ do is align points are near each other in both axes with a higher threshold, then do the global `x` and `y` alignment at the lower threshold.

**Trim Flash Properly**

Some phases output pieces like 

      --------------------
    -----------------------
     --   -    ----   -   --
          -           -   -

where we'd really just want

      --------------------
    -----------------------

or maybe

      --------------------
    -----------------------
               ----         

Try a naive cellular-automaton-based approach first, though you'll very probably need to vary rules based on direction of the island. You might initially try something like "Any living cell with more than one Von Neumann neighbor survives", or "Any living cell with more than three Moore neighbors survives, and any cell with 7 or more neighbors becomes alive" and see how that goes.

**Deal with overlaps**

We need to do is deal with overlapping elements somehow. In particular, if a previous processing step outputs something like

    A               B
    +----------+----+----------+
               C               D

where line segments `A->B` and `C->D` have the same slope and overlap partially, we'd really like to replace them with the line segment `A->D` instead. The equivalent applies to lines going in all directions.

**More input types**

We currently support `.ppm`, `.pgm` and `.txt` (ascii art) files (and fairly naively at that; we can _only_ read the [`RAWBITS` variants](http://www.fileformat.info/format/pbm/egff.htm#PBM-DMYID.3.5) of `.ppm`/`.pgm`). It'd be nice to have `.jpg` and `.png` reading support. The Haskell ecosystem fights us on this, because what we really want coming out the other side is a sparse representation (as of this writing, a `Map Coord a`), rather than a `Vector`. The libraries I've been able to find either don't work outright ([`unm-hip`](https://hackage.haskell.org/package/unm-hip-0.3.1.6/docs/Data-Image-Boxed.html#t:BoxedImage) throws what look like pretty deep errors when I try to get the pixel vector out of one of its images), or work really _really_ hard to prevent you from getting at raw pixel data ([`JuicyPixels`](http://hackage.haskell.org/package/JuicyPixels) provides a lot of infrastructure for mapping over pixel information, but they all keep the output locked into the `DynamicImage` type, which does us no good whatsoever in this project).

**Text support**

Text is currently unsupported. I'm thinking we can do a line-art processing step, followed by running occupied non-line areas through something like [`tesseract`](http://code.google.com/p/tesseract-ocr/) to get textual data.

**Better performance**

At the moment, crunching a ~1mb file takes a few seconds. Ideally, it would be fast enough to support interactive conversion speeds, but I'd settle for sub-second processing of most target files. Also, note that the use case for this project is to be step 1 of `n` in a visual compilation pipeline. Which means we can't really afford to be slow if we want that style of compilation to be bearable.

### General Notes

- These are not experiments aimed at generic object identification. The idea here is to take dead bitmaps and generate flow diagrams for [visual programs](http://langnostic.inaimathi.ca/article?name=the-big-problem-and-visual-compilers.html) from them. This means we're dealing with a very restricted set of images:
	- All white background
	- Some sparse text
	- Only line drawings
- The idea is to get something practically workable, and not necessarily start out with the general case solution. In particular, I'm perfectly willing to "cheat" by adding the restriction that lines/arrows and shapes be represented in different colors. So as far as I'm concerned, another perfectly legitimate example is something like [this](https://github.com/Inaimathi/EAF/blob/master/test-data/multi-color.txt). At the current level of experimentation, it doesn't seem to make as big a difference as I thought it might (as you can see by comparing [this](https://github.com/Inaimathi/EAF/blob/master/test-data/single-color.txt)->[this](https://github.com/Inaimathi/EAF/blob/master/test-data/single-color.svg) to [this](https://github.com/Inaimathi/EAF/blob/master/test-data/multi-color.txt)->[this](https://github.com/Inaimathi/EAF/blob/master/test-data/multi-color.svg).) There's really no reason _not_ to support it, since the exact same machinery can handle single and multi-color inputs, but I don't think I'll be putting emphasis on color coded images for the moment.
- It might seem that thinning Cardinal lines by finding the longest contiguous line would be a pretty good idea, because it would reduce flash. Unfortunately, it also tends to truncate long, _almost_ straight lines drawn by hand. It seems that Cardinal lines should be thinned exactly like Ordinal lines; by doing a mild transformation on their bounding box. This is probably not true for the sorting algorithm though; while condensing, we probably do want to treat cardinal lines as the bounding box of their longest contained, contiguous line.
