// --------------------------------------------------------------------------
//  CAdaptiveCaching::_processPixelOverlap()
// --------------------------------------------------------------------------
template<class TRecord,class TContribArray>
int
CAdaptiveCaching::_processPixelOverlap(const CHitInfoLite &hit,
                                       const float *directIllum,
                                       const float  thresholdElevation,
                                       TContribArray &carray,
                                       CRCOctree<TRecord> *octree)
{
}

if(_adaptive && !pixContribs.ProcessPixelDone())
  {
    _extrapolateRadianceValid(pixContribs,hit);
    _processPixelOverlap(hit,hit.directIllum.data(),
                         1/*thrElevImage[idx]*/,pixContribs,GetOctree<TRecord>());
    pixContribs.SetProcessPixelDone();
  }
