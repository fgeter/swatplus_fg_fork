      subroutine pest_enrsb

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the enrichment ratio for nutrient and
!!    pesticide transport with runoff

      use hru_module, only : hru, surfq, sedyld, sanyld, silyld, clayld, sagyld, lagyld, ihru, enratio
      
      implicit none       
      
      integer :: j = 0       !none          |HRU number
      real :: cy = 0.        !              |

      j = ihru

      if (sedyld(j) < 1.e-4) then
        sedyld(j) = 0.0
        sanyld(j) = 0.0
        silyld(j) = 0.0
        clayld(j) = 0.0
        sagyld(j) = 0.0
        lagyld(j) = 0.0
      endif

      !! CREAMS method for calculating enrichment ratio
      cy = .1 * sedyld(j) / (hru(j)%area_ha * surfq(j) + 1.e-6)
      if (cy > 1.e-6) then
        enratio = .78 * cy ** (-.2468)
      else
        enratio = 0.
      endif

      if (enratio > 3.0) enratio = 3.0

      return
      end subroutine pest_enrsb