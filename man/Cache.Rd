% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caching.R
\name{Cache}
\alias{Cache}
\title{Cache class for object, file or directory caching}
\description{
A class to handle caching of objects, files, or directories.
It generates a signature from the provided object and creates cached versions
based on that signature.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{cache_dir}}{Directory where cached files are stored}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Cache-new}{\code{Cache$new()}}
\item \href{#method-Cache-clear}{\code{Cache$clear()}}
\item \href{#method-Cache-clear_all}{\code{Cache$clear_all()}}
\item \href{#method-Cache-get}{\code{Cache$get()}}
\item \href{#method-Cache-is_cached}{\code{Cache$is_cached()}}
\item \href{#method-Cache-get_path}{\code{Cache$get_path()}}
\item \href{#method-Cache-save}{\code{Cache$save()}}
\item \href{#method-Cache-clone}{\code{Cache$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cache-new"></a>}}
\if{latex}{\out{\hypertarget{method-Cache-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new Cache object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cache$new(
  sig_object,
  prefix,
  cache_dir,
  save_sig = FALSE,
  kind = c("object", "file", "dir")
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sig_object}}{Object used to generate the cache signature}

\item{\code{prefix}}{Prefix for the cache filename}

\item{\code{cache_dir}}{Directory where cached files are stored}

\item{\code{save_sig}}{Whether to save the signature to a file}

\item{\code{kind}}{Type of cache: "object", "file", or "dir"}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new Cache object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cache-clear"></a>}}
\if{latex}{\out{\hypertarget{method-Cache-clear}{}}}
\subsection{Method \code{clear()}}{
Clear the current cached object/file/directory
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cache$clear()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cache-clear_all"></a>}}
\if{latex}{\out{\hypertarget{method-Cache-clear_all}{}}}
\subsection{Method \code{clear_all()}}{
Clear all cached objects in the cache directory
Be careful with this operation as it will remove all cached files in the cache directory
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cache$clear_all()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cache-get"></a>}}
\if{latex}{\out{\hypertarget{method-Cache-get}{}}}
\subsection{Method \code{get()}}{
Retrieve the cached object/file/directory
When NULL is returned, it means the cache does not exist
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cache$get(target = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{target}}{Target path for file/directory cache (required for file/dir kind)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
For objects: the cached object or NULL if not found
For files/dirs: the target path or NULL if not found
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cache-is_cached"></a>}}
\if{latex}{\out{\hypertarget{method-Cache-is_cached}{}}}
\subsection{Method \code{is_cached()}}{
Check if we have a cached object/file/directory
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cache$is_cached()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
TRUE if the cached object/file/directory exists, FALSE otherwise
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cache-get_path"></a>}}
\if{latex}{\out{\hypertarget{method-Cache-get_path}{}}}
\subsection{Method \code{get_path()}}{
Get the path to the cached object/file/directory
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cache$get_path()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The path to the cached object/file/directory
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cache-save"></a>}}
\if{latex}{\out{\hypertarget{method-Cache-save}{}}}
\subsection{Method \code{save()}}{
Save an object/file/directory to cache
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cache$save(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{The object to cache, or path to file/directory to cache}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Cache-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Cache-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Cache$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
