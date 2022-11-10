////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  This file is part of NCrystal (see https://mctools.github.io/ncrystal/)   //
//                                                                            //
//  Copyright 2015-2022 NCrystal developers                                   //
//                                                                            //
//  Licensed under the Apache License, Version 2.0 (the "License");           //
//  you may not use this file except in compliance with the License.          //
//  You may obtain a copy of the License at                                   //
//                                                                            //
//      http://www.apache.org/licenses/LICENSE-2.0                            //
//                                                                            //
//  Unless required by applicable law or agreed to in writing, software       //
//  distributed under the License is distributed on an "AS IS" BASIS,         //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  //
//  See the License for the specific language governing permissions and       //
//  limitations under the License.                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#include "NCrystal/NCMem.hh"
#include "NCrystal/NCDefs.hh"
#include <vector>
#include <mutex>

namespace NC = NCrystal;

namespace NCrystal {
  namespace {
    static std::mutex s_cacheCleanerMutex;
    static std::vector<std::function<void()>> s_cacheCleanerMutexFcts;
  }
}

void NC::clearCaches()
{
  NCRYSTAL_LOCK_GUARD(s_cacheCleanerMutex);
  for (auto& f : s_cacheCleanerMutexFcts)
    f();
}

void NC::registerCacheCleanupFunction( std::function<void()> f )
{
  NCRYSTAL_LOCK_GUARD(s_cacheCleanerMutex);
  s_cacheCleanerMutexFcts.emplace_back(f);
}

void * NC::detail::bigAlignedAlloc( std::size_t alignment, std::size_t size )
{
  void * result = nullptr;

  //Aligned_alloc available in C++17, but on OSX only from Catalina (10.15):
#if __cplusplus >= 201703L
#  if defined(__APPLE__)
#    if defined(__MACH__) && defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 101500
#      define NCRYSTAL_DETAIL_SYSHASALIGNEDALLOC
#    endif
#  else
#    define NCRYSTAL_DETAIL_SYSHASALIGNEDALLOC
#  endif
#endif

#ifdef NCRYSTAL_DETAIL_SYSHASALIGNEDALLOC
  struct detail {
    static std::size_t roundToNextMultiple( std::size_t i, std::size_t n )
    {
      nc_assert(n>0);
      auto remainder = i % n;
      if (!remainder)
        return i;
      return (i + n) - remainder;
    }
  };
  //Observed issues with std::aligned_alloc on osx, seems to simply be missing
  //from std:: namespace:
  //    result = std::aligned_alloc(alignment,size);

  //roundToNextMultiple since that is what the aligned_alloc function expects.
#ifdef WIN32
#define aligned_alloc _aligned_malloc
#endif
  result = aligned_alloc(alignment,detail::roundToNextMultiple( size, alignment ));
#endif
  if ( result == nullptr ) {
    //Try to over allocate and then trim off something:
    std::size_t sa = size+alignment;
    void * buf = std::malloc(sa);
    if ( buf != nullptr ) {
#  if defined(__GNUC__) && (__GNUC__*1000+__GNUC_MINOR__)<5001
      //std::align also missing on old gcc. Workaround from
      //https://gcc.gnu.org/bugzilla/show_bug.cgi?id=57350:
      std::uintptr_t pn = reinterpret_cast< std::uintptr_t >( buf );
      result = reinterpret_cast< void * >( ( pn + alignment - 1 ) & - alignment );
#  else
      result = std::align(alignment,size,buf,sa);
#  endif
    }
  }
  if ( result == nullptr )
    throw std::bad_alloc();
  return result;
}
