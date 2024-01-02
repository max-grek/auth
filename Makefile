include makefiles/base.Makefile

# build args
SERVICE_NAME	= auth-api

# http args
HTTP_HOST	= 0.0.0.0
HTTP_PORT 	= 9000
HTTP_TIMEOUT	= 10s

all: create run

.PHONY: all

setup:
	@git submodule init
	@git submodule update --remote docker-flows
	@git submodule update --remote makefiles

setup-env:
	@echo "PG_CONTEXT=/home/samurai/dev/git.amceo.com/infrastructure/db-vault" > /tmp/.env
	@echo "REGISTRY=$(REGISTRY)" >> /tmp/.env

env-up:
	@$(MAKE) -f $(DOCKER_PATH) env-up \
		FLOW=auth \
		ENV=/tmp/.env \
		REBUILD=--build \
		\
		IP_ADDR=$(IP_ADDR) \
		GATEWAY_ADDR=$(GATEWAY_ADDR) \
		CHECK_FOR_NET_EXISTS=$(CHECK_FOR_NET_EXISTS) \
		NET_NAME=$(NET_NAME)
