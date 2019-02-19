# This file is used by ndk-build.
# See https://developer.android.com/ndk/guides/application_mk .
#
# Note: CGE libraries are "prebuilt" (since we use FPC to do the actual building).
# However, we cannot avoid using ndk-build to define them (otherwise creating
# package complains that we have native code but don't use native build system).
# In the past, using ndk-build also allowed smooth integration with ndk-gdb
# (we could see Pascal symbols and set breakpoints on them),
# but this doesn't work anymore.
#
# Note: We do not specify architectures here using APP_ABI.
# Instead they are listed in build.gradle as ndk.abiFilters.
# The docs https://developer.android.com/ndk/guides/application_mk
# say explicitly that APP_ABI in Application.mk is ignored then.

# NDK platform version should in practice always equal
# minSdkVersion, or be lower than it.
# But it doesn't really make any sense to set it lower,
# so in practice it should be equal.
# NDK platform version must *never* be higher than minSdkVersion
# ( https://stackoverflow.com/questions/21888052/what-is-the-relation-between-app-platform-androidminsdkversion-and-androidtar ).
APP_PLATFORM := android-16
