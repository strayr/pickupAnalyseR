#syscompPickup
source('pickup.R')
SyscompPickup <- setRefClass(
  "SyscompPickup",
  contains="Pickup"
)