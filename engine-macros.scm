(module engine-macros (trans)
  (import scheme chicken data-structures)
    (define-syntax trans
      (syntax-rules () 
        [(_ str args ...) (string-translate$ str args ...)]))
  );end of module
