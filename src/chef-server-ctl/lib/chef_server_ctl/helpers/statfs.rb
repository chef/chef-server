require "ffi" unless defined?(FFI)

class Statfs
  #
  # Statfs provides a simple interface to the statvfs system call.
  # Since the statvfs struct varies a bit across platforms, this
  # likely only works on Linux and OSX at the moment.
  #
  extend FFI::Library
  ffi_lib FFI::Library::LIBC

  attach_function(:statvfs, %i{string pointer}, :int)
  attach_function(:strerror, [:int], :string)

  FSBLKCNT_T = if RbConfig::CONFIG["host_os"] =~ /darwin|osx|mach/i
                 :uint
               else
                 :ulong
               end

  # See http://man7.org/linux/man-pages/man2/statvfs.2.html
  class Statvfs < FFI::Struct
    spec = [
            :f_bsize,   :ulong,      # Filesystem block size
            :f_frsize,  :ulong,      # Fragement size
            :f_blocks,  FSBLKCNT_T,  # Size of fs in f_frsize units
            :f_bfree,   FSBLKCNT_T,  # Number of free blocks
            :f_bavail,  FSBLKCNT_T,  # Number of free blocks for unpriviledged users
            :f_files,   FSBLKCNT_T,  # Number of inodes
            :f_ffree,   FSBLKCNT_T,  # Number of free inodes
            :f_favail,  FSBLKCNT_T,  # Number of free inodes for unprivilged users
            :f_fsid,    :ulong,      # Filesystem ID
            :f_flag,    :ulong,      # Mount Flags
            :f_namemax, :ulong       # Max filename length
           ]

    # Linux has this at the end of the struct and if we don't include
    # it we end up getting a memory corruption error when th object
    # gets GCd.
    if RbConfig::CONFIG["host_os"] =~ /linux/i
      spec << :f_spare
      spec << [:int, 6]
    end

    layout(*spec)
  end

  def initialize(path)
    @statvfs = stat(path)
  end

  #
  # @returns [Integer] Free inodes on the given filesystem
  #
  def free_inodes
    @statvfs[:f_favail]
  end

  #
  # @returns [Integer] Free space in KB on the given filesystem
  #
  def free_space
    # Since we are running as root we could report f_bfree but will
    # stick with f_bavail since it will typically be more
    # conservative.
    (@statvfs[:f_frsize] * @statvfs[:f_bavail]) / 1024
  end

  private

  def stat(path)
    statvfs = Statvfs.new
    if statvfs(path, statvfs.to_ptr) != 0
      raise "StatvfsFailedException", command.stderr
    end

    statvfs
  end
end
