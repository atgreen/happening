Name:           happening
Version:        0.4.0
Release:        1%{?dist}
Summary:        A privacy-focused, self-hosted web analytics platform

License:        MIT
URL:            https://github.com/atgreen/happening
Source0:        happening-%{version}.tar.gz

# Disable debug packages and stripping since this is a Lisp binary with dumped image
%global debug_package %{nil}
%global _build_id_links none
%global __strip /bin/true
%global __brp_strip %{nil}
%global __brp_strip_comment_note %{nil}
%global __brp_strip_static_archive %{nil}

BuildRequires:  sbcl
BuildRequires:  libfixposix-devel
BuildRequires:  gcc
BuildRequires:  make

%description
Happening is a privacy-focused, self-hosted web analytics platform built
in Common Lisp. It provides lightweight, cookie-free website analytics
with a focus on data ownership and user privacy.

%prep
%autosetup

%build
make

%install
# Install the binary
install -D -m 0755 happening %{buildroot}%{_bindir}/happening

# Install SBOM
install -D -m 0644 happening-sbom.spdx.json %{buildroot}%{_datadir}/sbom/happening-%{version}.spdx.json

%files
%license LICENSE
%doc README.md
%{_bindir}/happening
%{_datadir}/sbom/happening-%{version}.spdx.json

%changelog
* Fri Apr 18 2026 Anthony Green <green@moxielogic.com> - 0.4.0-1
- Initial RPM package for happening
