
##########################
# DOCUMENTATION HELPER METHODS
# -----------------------
# 
# Helper functions used internally for documentation generation
# For development use only, do not call them directly
# Imports roxygen2 package. Not used in the release edition
# 
# ########################
# Copyright (C) 2020 Nunes IJG et al


# Reads the source code lines to help documentation processing
read.doc.tag <- local({
  self = sys.frame(-1L)
  self$last_declared_s4class = NULL
  self$last_declared_s4method = NULL
  self$last_read_file = NULL
  self$last_read_lines = NULL
  self$last_method_category = ""
  self$method_descriptors = list() # Items are lists with $usage, $description, and $category
  self$roxy = NULL
  function(x)
  {
    if (is.null(x)) return(self$last_declared_s4class)
    if (is.null(self$last_read_file) || (self$last_read_file != x$file))
    {
      self$last_read_file = x$file
      self$last_read_lines = readLines(x$file)
    }
    if (is.null(self$roxy))
    {
      self$roxy = environment(eval(str2lang('roxygen2::roxygenize')))
    }
    fndecl = parse(text = self$last_read_lines[seq.int(x$line, length(self$last_read_lines))], n = 1L)[[1]]
    fndecl
  }
})

# Enables roxygen2's @declareS4class tag
roxy_tag_parse.roxy_tag_declareS4class <- local({
  self = environment(read.doc.tag)$self
  function(x)
  {
    self = environment(read.doc.tag)$self
    fndecl = read.doc.tag(x)
    if (as.character(fndecl[[1]]) != "setClass") return(NULL)
    clname = fndecl[[2]]
    stopifnot(nchar(clname) > 0)
    self$last_declared_s4class = clname
    rdname = sprintf("%s-class", clname)
    code = c(
      sprintf('@return A [`%s`] object', rdname),
      sprintf('@name %s', rdname),
      sprintf('@rdname %s', rdname),
      "@export"
    )
    self$last_method_category = ""
    self$last_declared_s4method = NULL
    x$raw = sprintf("c(%s)", paste0(sprintf('"%s"', code), collapse = ", "))
    x$tag = "eval"
    self$roxy$tag_code(x)
  }
})

# Enables roxygen2's @s4method tag
roxy_tag_parse.roxy_tag_s4method <- function(x, mtype="s4")
{
  self = environment(read.doc.tag)$self
  clname = self$last_declared_s4class
  if (is.null(clname)) clname = basename(x$file)
  rdname = sprintf("%s-class", clname)
  code = c(
    sprintf('@rdname %s', rdname),
    "@usage  ",
    "@export"
  )
  formatfn_usage = self$roxy$function_usage
  rdfn = self$roxy$build_rd
  if (!is.function(formatfn_usage)) return(NULL)
  fndecl = read.doc.tag(x)
  if (length(fndecl) == 0) return(NULL)
  fncallname = as.character(fndecl[[1]])
  if (fncallname %in% c('<-', '='))
  {
    fname = as.character(fndecl[[2]])
    fname2 = fname
    if (isS3method(fname))
      fname = trimws(as.character(fndecl[[2]]), 'right', sprintf("\\.%s", clname))
    fnusage = formatfn_usage(fname, formals(as.character(fndecl[[2]])))
    fndecl = as.call(list(as.symbol("setMethod"), f = fname, signature = clname))
    fnbasename = fname
    fname = fname2
  }
  else if (fncallname %in% 'setMethod')
  {
    fndecl = match.call(eval(fndecl[[1]]), fndecl)
    if (is.call(fndecl$signature))
      fndecl$signature = eval(fndecl$signature)
    fname = as.character(fndecl$f)
    fnusage = switch(fname,
                     `[` = sprintf("x[i, j, ..., drop=%s]", formals(getMethod(fndecl$f, fndecl$signature))$drop),
                     `[<-` = "x[i, j, ...] <- value",
                     `$` = "x$name <- value",
                     `$<-` = "x$name <- value",
                     formatfn_usage(fname, formals(getMethod(fndecl$f, fndecl$signature)))
                     )
    if (identical(fnusage, "character"))
      fnusage = rdfn(fnusage)
    fname = paste0(fname, paste0(fndecl$signature, collapse=','))
    fnbasename = fname
  }
  self$last_declared_s4method = fndecl
  desc = (if (is.character(x$raw)) trimws(c(x$raw, ""))[[1]] else "")
  if (nchar(desc) == 0L)
  {
    if (mtype %in% "s3")
    {
      desc = if (startsWith(fnbasename, "as."))
      {
        if (endsWith(fname, sprintf(".%s", clname)))
          sprintf("Converts this object to `%s`", trimws(fnbasename, "left", "^as\\."))
        else if (startsWith(fname, sprintf("as.%s.", clname)))
          sprintf("Converts a `%s` to a `%s`", trimws(fname, "left", sprintf("^as\\.%s\\.", clname)), clname)
        else
          sprintf("Converts to `%s`", trimws(fnbasename, "left", "^as\\."))
      }
      else
        sprintf("Generic `%s` implementation for `%s`", fnbasename, clname)
    }
    else if (mtype %in% "s4")
    {
      desc = switch (fnbasename,
        show = "Prints the overview description about the contents in `object`",
        plot = "Plots the object's contents",
        `[` = "Extracts the elements from the primary table",
        `$` = "Extracts an element from the primary list",
        ""
      )
    }
  }
    
  if (nchar(desc) != 0L)
  {
    desc = gsub("\\", "\\\\", desc, fixed=TRUE, useBytes=TRUE)
    desc = gsub("%", "\\%", desc, fixed=TRUE, useBytes=TRUE)
    code = c(sprintf("@%smethodDescription %s %s", mtype, fname, paste0(desc, collapse=" \\\\cr ")), code)
  }
  self$method_descriptors[[fname]] = list(
    usage = fnusage,
    description = desc,
    category = self$last_method_category
  )
  x$raw = sprintf("c(%s)", paste0(sprintf('"%s"', code), collapse = ", "))
  x$tag = "eval"
  self$roxy$tag_code(x)
}

# Same as @s4method, but for s3 methods
roxy_tag_parse.roxy_tag_s3method <- function(x)
{
  x = roxy_tag_parse.roxy_tag_s4method(x, mtype="s3")
  x
}


# Enables roxygen2's @s4methodDescription tag (for internal use, do not apply this tag in the script)
roxy_tag_parse.roxy_tag_s4methodDescription <- function(x)
{
  self = environment(read.doc.tag)$self
  xval = self$roxy$tag_two_part(x, "name", "description", required = FALSE)$val
  fname = xval$name
  metinfo = self$method_descriptors[[fname]]
  if (is.null(metinfo))
  {
    return(NULL)
  }
  x$raw = xval$description
  x$method_name = fname
  x$method_usage = metinfo$usage
  x = self$roxy$tag_markdown(x)
  x$cat = metinfo$category
  x$method_name = fname
  x
}

# Internal use. Same as s4methodDescription, but for S3 methods
roxy_tag_parse.roxy_tag_s3methodDescription <- function(x)
{
  self = environment(read.doc.tag)$self
  x = roxy_tag_parse.roxy_tag_s4methodDescription(x)
  if (!is.null(x))
  {
    if(nchar(x$cat) == 0L)
      x$cat = "S3 Methods"
  }
  x
}

# Parses descriptions as implementation items in "Methods" section
roxy_tag_rd.roxy_tag_s4methodDescription <- function(tag, env, base_path)
{
  if (is.null(tag)) return(NULL)
  self = environment(read.doc.tag)$self
  x = self$roxy$rd_section("methods", sprintf("\\item{\\code{%s}}{%s}", tag$method_usage, tag$val))
  x$name = c(tag$method_name, "")[[1]]
  x$cat = c(tag$cat, "")[[1]]
  x
}

roxy_tag_rd.roxy_tag_s3methodDescription <- roxy_tag_rd.roxy_tag_s4methodDescription

# Enables roxygen2's @methodsnote tag
roxy_tag_parse.roxy_tag_methodsnote <- function(x)
{
  if (is.null(x)) return(NULL)
  self = environment(read.doc.tag)$self
  self$roxy$tag_markdown(x)
}

roxy_tag_rd.roxy_tag_methodsnote <- function(tag, env, base_path)
{
  if (is.null(tag)) return(NULL)
  self = environment(read.doc.tag)$self
  x = self$roxy$rd_section("methods", tag$val)
  x$name = ""
  x$cat = ""
  x
}

# Enables roxygen2's @s4accessor tag
roxy_tag_parse.roxy_tag_s4accessor <- function(x)
{
  self = environment(read.doc.tag)$self
  if (is.null(self$last_declared_s4method)) return(NULL)
  fndecl = self$last_declared_s4method
  fname = as.character(fndecl$f)
  clname = self$last_declared_s4class
  metinfo = self$method_descriptors[[fname]]
  if (is.null(metinfo)) return(NULL)
  desc = metinfo$description
  if (x$raw == "")
    slotnm = trimws(fname, 'right', '<-')
  else
  {
    x$raw = sub('^(\\S+\\s*?)\\r?\\n', "\\1 \\\\cr ", trimws(x$raw), perl = TRUE)
    xval = self$roxy$tag_two_part(x, "name", "description", required = FALSE)$val
    desc = trimws(sprintf("%s %s", xval[["description"]], desc))
    if (any(nchar(desc) != 0L))
      desc = sprintf(". %s", desc)
    slotnm = xval[["name"]]
  }
  is_setter = endsWith(fname, "<-")
  x$accessor_type = if(is_setter) "set" else "get"
  valname = if (is_setter) "value" else na.exclude(c(getSlots(getClassDef(clname))[slotnm], 'value'))[[1]]
  x$raw = sprintf("%s \\code{%s} slot%s", sprintf(if(is_setter) "Sets a %s to the" else "Gets the `%s` from the", valname), slotnm, desc)
  x$method_name = fname
  x$method_usage = metinfo$usage
  x = self$roxy$tag_markdown(x)
  metinfo$category = "Slot accessors"
  x$cat = metinfo$category
  self$method_descriptors[[fname]] = metinfo
  x
}

roxy_tag_rd.roxy_tag_s4accessor <- roxy_tag_rd.roxy_tag_s4methodDescription

# Enables roxygen2's @category tag
roxy_tag_parse.roxy_tag_category <- function(x)
{
  self = environment(read.doc.tag)$self
  x = self$roxy$tag_value(x)
  xcat = x$val
  if (is.null(xcat)) xcat = ""
  self$last_method_category = xcat
  x
}

# Enables roxygen2's @options tag
roxy_tag_parse.roxy_tag_options <- function(x)
{
  self = environment(read.doc.tag)$self
  fndecl = read.doc.tag(x)
  if (length(fndecl) != 3L) return(NULL)
  fndecl = call(as.character(fndecl[[1]]), fndecl[[2]], eval(fndecl[[3]]))
  rdname = if (trimws(x$raw) != "")
    trimws(self$roxy$tag_value(x)$val, whitespace = '[\\[\\]]')
  else
    sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x$file))
  fmtargs = deparse(fndecl[[3]], width.cutoff = 50L)
  
  fmtargs[-1] = paste0("  ", fmtargs[-1])
  fnfmt = sprintf("%s\n%s", as.character(fndecl[[2]]), paste0("# ", fmtargs, collapse="\n"))
  fnfmt = gsub("\\", "\\\\", fnfmt, fixed=TRUE, useBytes=TRUE)
  fnfmt = gsub("%", "\\%", fnfmt, fixed=TRUE, useBytes=TRUE)
  fnfmt = gsub("\"", "\\\"", fnfmt, fixed=TRUE, useBytes=TRUE)
  code = c(
    sprintf('@usage %s', fnfmt),
    "@format",
    sprintf('@rdname %s', rdname),
    "@export"
  )
  x$raw = sprintf("c(%s)", paste0(sprintf('"%s"', code), collapse = ", "))
  x = self$roxy$tag_code(x)
  x$tag = "eval"
  x
}

merge.rd_section_methods <- function(x, y, ...)
{
  stopifnot(identical(class(x), class(y)))
  if (is.null(x$cat)) x$cat = ""
  if (is.null(y$cat)) y$cat = ""
  if (is.null(x$name)) x$name = ""
  if (is.null(y$name)) y$name = ""
  self = environment(read.doc.tag)$self
  z = self$roxy$merge.rd_section(x, y, ...)
  if(is.null(z)) return(NULL)
  z$cat = c(x$cat, y$cat)
  z$name = c(x$name, y$name)
  ord = order(z$name, z$cat)
  z$value = z$value[ord]
  z$cat = z$cat[ord]
  z$name = z$name[ord]
  z
}

format.rd_section_methods <- function(x, ...)
{
  sel_notes = (x$name == "") & (x$cat == "")
  noteitems = x$value[sel_notes]
  if (length(noteitems) != 0L)
    noteitems = c(noteitems, "")
  x$value = x$value[!sel_notes]
  x$cat = x$cat[!sel_notes]
  x$name = x$name[!sel_notes]
  fmtitems = unlist(lapply(split(x$value, x$cat), paste0, collapse='\n'))
  catitems = names(fmtitems)
  catitems[catitems != ""] = sprintf("\\sspace\\cr\\strong{%s}\n", catitems[catitems != ""])
  fmtitems = sprintf("%s\\describe{\n%s\n}", catitems, fmtitems)
  sprintf("\\section{Methods}{\n%s\n%s\n}\n", paste0(noteitems, collapse = "\\cr \n"), paste0(fmtitems, collapse = "\n"))
}


