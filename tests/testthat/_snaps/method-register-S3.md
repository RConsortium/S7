# S3 method registration / rejects class_missing on S3 generics

    Code
      method(s3_gen, class_missing) <- (function(x) "missing")
    Condition
      Error:
      ! `class_missing` not supported for non-operator S3 generics.

# S3 method unregistration / errors when unregistering from an S3 generic

    Code
      method(sum, foo) <- NULL
    Condition
      Error in `method<-`:
      ! Can't unregister methods for S3 generics

---

    Code
      method(base_sum, foo) <- NULL
    Condition
      Error in `method<-`:
      ! Can't unregister methods for S3 generics

