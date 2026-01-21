# typed: false
# frozen_string_literal: true

class Surreal < Formula
  desc "Programming language with Rust-like syntax and Erlang-style concurrency"
  homepage "https://github.com/surreal-lang/surreal"
  version "0.1.0"
  license "MIT"

  on_macos do
    on_arm do
      url "https://github.com/surreal-lang/surreal/releases/download/v#{version}/surreal-aarch64-apple-darwin.tar.gz"
      # sha256 will be filled in by the release workflow
      sha256 "PLACEHOLDER_SHA256_ARM64"
    end

    on_intel do
      url "https://github.com/surreal-lang/surreal/releases/download/v#{version}/surreal-x86_64-apple-darwin.tar.gz"
      # sha256 will be filled in by the release workflow
      sha256 "PLACEHOLDER_SHA256_X64"
    end
  end

  on_linux do
    on_arm do
      url "https://github.com/surreal-lang/surreal/releases/download/v#{version}/surreal-aarch64-unknown-linux-gnu.tar.gz"
      sha256 "PLACEHOLDER_SHA256_LINUX_ARM64"
    end

    on_intel do
      url "https://github.com/surreal-lang/surreal/releases/download/v#{version}/surreal-x86_64-unknown-linux-gnu.tar.gz"
      sha256 "PLACEHOLDER_SHA256_LINUX_X64"
    end
  end

  def install
    bin.install "surreal"
  end

  test do
    # Test that the binary runs
    assert_match "surreal", shell_output("#{bin}/surreal --version")
  end
end
