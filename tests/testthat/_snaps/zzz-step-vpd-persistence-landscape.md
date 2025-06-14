# recipe and preparation printing is consistent

    Code
      print(vpd_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Operations 
      * persistent features from a Rips filtration of: dist
      * persistence landscape of: dist_pd

---

    Code
      prep(vpd_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 2
      
      -- Training information 
      Training data contained 1 data points and no incomplete rows.
      
      -- Operations 
      * persistent features from a Vietoris-Rips filtration of: <none> | Trained
      * persistence landscape of: <none> | Trained

# `bake()` method errs needed non-standard role columns are missing

    Code
      bake(vpd_prep, new_data = subset(dist_test, select = -c(dist)))
    Condition
      Error in `step_pd_point_cloud()`:
      ! The following required column is missing from `new_data`: dist.

