Name:           A
Version:        1
Release:        1%{?dist}
Summary:        package A
License:        GPL
BuildRequires:  B

%description
Test package A

%package -n C-A
Summary: A for C

%description -n C-A
A support for C

%changelog
* Wed Jul 29 13:38:27 +08 2020 Jens Petersen <petersen@redhat.com>
-
