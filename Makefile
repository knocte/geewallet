all:
	@./scripts/make.sh

install:
	@./scripts/make.sh install

check:
	@./scripts/make.sh check

check-end2end:
	@./scripts/make.sh check-end2end

release:
	@./scripts/make.sh release

zip:
	@./scripts/make.sh zip

run:
	@./scripts/make.sh run

update-servers:
	@./scripts/make.sh update-servers

nuget:
	@./scripts/make.sh nuget

sanitycheck:
	@./scripts/make.sh sanitycheck

strict:
	@./scripts/make.sh strict
