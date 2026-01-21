# Homebrew Tap for Surreal

This directory contains the Homebrew formula for the Surreal programming language.

## Setup

This directory should be published as a separate repository: `surreal-lang/homebrew-surreal`

## Installation

```bash
brew tap surreal-lang/surreal
brew install surreal
```

## Updating the Formula

After a new release:

1. Update the version in `Formula/surreal.rb`
2. Calculate SHA256 checksums for each platform:
   ```bash
   shasum -a 256 surreal-aarch64-apple-darwin.tar.gz
   shasum -a 256 surreal-x86_64-apple-darwin.tar.gz
   shasum -a 256 surreal-aarch64-unknown-linux-gnu.tar.gz
   shasum -a 256 surreal-x86_64-unknown-linux-gnu.tar.gz
   ```
3. Update the `sha256` values in the formula
4. Commit and push

## Automated Updates

Consider using [homebrew-releaser](https://github.com/Justintime50/homebrew-releaser) or
a GitHub Action to automatically update the formula when new releases are created.
