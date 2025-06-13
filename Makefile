# Simple Gambit Builder Makefile
.PHONY: help build run setup build-all status shell clean create-volumes

DOCKER_IMAGE := gambit-simple
DOCKER_TAG := latest
CONTAINER_NAME := gambit-simple-container
DATA_VOLUME := gambit-data
ENV_VOLUME := gambit-env

# Colors
BLUE := \033[34m
GREEN := \033[32m
YELLOW := \033[33m
RED := \033[31m
RESET := \033[0m

help:
	@echo "$(BLUE)Simple Gambit Builder$(RESET)"
	@echo ""
	@echo "Builds 4 essential configurations:"
	@echo "  • v4.9.6-gcc14 (GCC 14, no flags)"
	@echo "  • v4.9.6-gcc14-sh (GCC 14, --enable-single-host)"
	@echo "  • v4.9.6-clang18 (Clang 18, no flags)"
	@echo "  • v4.9.6-clang18-sh (Clang 18, --enable-single-host)"
	@echo ""
	@echo "Commands:"
	@echo "  $(GREEN)build-image$(RESET)     - Build Docker image"
	@echo "  $(GREEN)setup$(RESET)           - Create and setup container"
	@echo "  $(GREEN)build-all$(RESET)       - Build all 4 configurations"
	@echo "  $(GREEN)status$(RESET)          - Show build status"
	@echo "  $(GREEN)shell$(RESET)           - Interactive shell with all versions"
	@echo "  $(GREEN)report$(RESET)          - Show build report"
	@echo "  $(GREEN)clean$(RESET)           - Clean up"
	@echo ""
	@echo "$(YELLOW)Note:$(RESET) Uses Docker volumes ($(DATA_VOLUME), $(ENV_VOLUME)) for improved build performance"
	@echo ""
	@echo "Quick start: make setup && make build-all && make shell"

create-volumes:
	@echo "$(BLUE)Creating Docker volumes...$(RESET)"
	@docker volume inspect $(DATA_VOLUME) >/dev/null 2>&1 || \
		(docker volume create $(DATA_VOLUME) && echo "$(GREEN)Created volume: $(DATA_VOLUME)$(RESET)")
	@docker volume inspect $(ENV_VOLUME) >/dev/null 2>&1 || \
		(docker volume create $(ENV_VOLUME) && echo "$(GREEN)Created volume: $(ENV_VOLUME)$(RESET)")

build-image:
	@echo "$(BLUE)Building simple Gambit image...$(RESET)"
	docker build -t $(DOCKER_IMAGE):$(DOCKER_TAG) .
	@echo "$(GREEN)Image built successfully!$(RESET)"

setup: build-image create-volumes
	@echo "$(BLUE)Setting up container with Docker volumes...$(RESET)"
	-docker stop $(CONTAINER_NAME) 2>/dev/null || true
	-docker rm $(CONTAINER_NAME) 2>/dev/null || true
	docker run -d \
		--name $(CONTAINER_NAME) \
		--cpus="$(shell nproc)" \
		--memory="8g" \
		--tmpfs /tmp:rw,noexec,nosuid,size=2g \
		--ulimit nofile=65536:65536 \
		-v $(DATA_VOLUME):/home/gambit/data \
		-v $(ENV_VOLUME):/home/gambit/.gambit_env \
		$(DOCKER_IMAGE):$(DOCKER_TAG) sleep infinity
	@echo "$(YELLOW)Initializing volume permissions as root...$(RESET)"
	docker exec --user root $(CONTAINER_NAME) sh -c 'mkdir -p /home/gambit/data /home/gambit/.gambit_env'
	docker exec --user root $(CONTAINER_NAME) chown -R gambit:gambit /home/gambit/data /home/gambit/.gambit_env
	@echo "$(BLUE)Checking if Gambit environment already exists...$(RESET)"
	@if docker exec $(CONTAINER_NAME) test -d /home/gambit/.gambit_env/bin; then \
		echo "$(GREEN)Gambit environment already set up, skipping setup.$(RESET)"; \
	else \
		echo "$(BLUE)Running Gambit environment setup...$(RESET)"; \
		docker exec -it $(CONTAINER_NAME) ./simple-builder.sh setup; \
	fi
	@echo "$(GREEN)Container ready with Docker volumes!$(RESET)"
	@echo "$(BLUE)Data persisted in volumes: $(DATA_VOLUME), $(ENV_VOLUME)$(RESET)"

build-all:
	@echo "$(BLUE)Building all configurations (using Docker volumes for speed)...$(RESET)"
	docker exec -it $(CONTAINER_NAME) ./simple-builder.sh build

status:
	docker exec -it $(CONTAINER_NAME) ./simple-builder.sh status

report:
	docker exec -it $(CONTAINER_NAME) ./simple-builder.sh report

shell:
	@echo "$(BLUE)Opening Gambit shell...$(RESET)"
	@echo "$(YELLOW)Commands:$(RESET)"
	@echo "  gambit env v4.9.6-gcc14    - Switch to GCC version"
	@echo "  gambit env v4.9.6-clang18  - Switch to Clang version"
	@echo "  deactivate                 - Return to default"
	@echo ""
	@echo "$(BLUE)Data volumes:$(RESET) $(DATA_VOLUME), $(ENV_VOLUME)"
	@echo ""
	docker exec -it $(CONTAINER_NAME) bash

# Build specific configurations
build-gcc:
	docker exec -it $(CONTAINER_NAME) ./simple-builder.sh single gcc14 default
	docker exec -it $(CONTAINER_NAME) ./simple-builder.sh single gcc14 sh

build-clang:
	docker exec -it $(CONTAINER_NAME) ./simple-builder.sh single clang18 default
	docker exec -it $(CONTAINER_NAME) ./simple-builder.sh single clang18 sh

# Container management
start:
	docker start $(CONTAINER_NAME)

stop:
	docker stop $(CONTAINER_NAME)

logs:
	docker exec $(CONTAINER_NAME) ls -la /home/gambit/.gambit_env/logs/

# Volume management
list-volumes:
	@echo "$(BLUE)Gambit Docker volumes:$(RESET)"
	@docker volume ls --filter name=gambit
	@echo ""
	@echo "$(BLUE)Volume details:$(RESET)"
	@docker volume inspect $(DATA_VOLUME) $(ENV_VOLUME) 2>/dev/null | grep -E '"Name"|"Mountpoint"' || echo "$(YELLOW)Volumes not yet created$(RESET)"

inspect-volumes:
	@echo "$(BLUE)Data volume ($(DATA_VOLUME)) contents:$(RESET)"
	@docker run --rm -v $(DATA_VOLUME):/data alpine ls -la /data 2>/dev/null || echo "$(YELLOW)Volume empty or not accessible$(RESET)"
	@echo ""
	@echo "$(BLUE)Environment volume ($(ENV_VOLUME)) contents:$(RESET)"
	@docker run --rm -v $(ENV_VOLUME):/env alpine ls -la /env 2>/dev/null || echo "$(YELLOW)Volume empty or not accessible$(RESET)"

# Backup volumes
backup-volumes:
	@echo "$(BLUE)Creating volume backups...$(RESET)"
	@mkdir -p ./backups
	docker run --rm -v $(DATA_VOLUME):/data -v $(PWD)/backups:/backup alpine tar czf /backup/gambit-data-$(shell date +%Y%m%d-%H%M%S).tar.gz -C /data .
	docker run --rm -v $(ENV_VOLUME):/env -v $(PWD)/backups:/backup alpine tar czf /backup/gambit-env-$(shell date +%Y%m%d-%H%M%S).tar.gz -C /env .
	@echo "$(GREEN)Backups created in ./backups/$(RESET)"

# Restore from backup (example - user needs to specify backup file)
restore-data:
	@echo "$(YELLOW)To restore from backup:$(RESET)"
	@echo "docker run --rm -v $(DATA_VOLUME):/data -v \$$(PWD)/backups:/backup alpine tar xzf /backup/BACKUP_FILE.tar.gz -C /data"

# Clean up options
clean-container:
	@echo "$(YELLOW)Cleaning up container (keeping volumes)...$(RESET)"
	-docker stop $(CONTAINER_NAME) 2>/dev/null || true
	-docker rm $(CONTAINER_NAME) 2>/dev/null || true

clean-image: clean-container
	@echo "$(YELLOW)Cleaning up image (keeping volumes)...$(RESET)"
	-docker rmi $(DOCKER_IMAGE):$(DOCKER_TAG) 2>/dev/null || true

clean-volumes:
	@echo "$(RED)Warning: This will delete all build data in volumes!$(RESET)"
	@echo "$(YELLOW)Volumes to be deleted: $(DATA_VOLUME), $(ENV_VOLUME)$(RESET)"
	@printf "Are you sure? [y/N] "; \
	read answer; \
	case "$$answer" in \
		[Yy]*) \
			echo "$(YELLOW)Removing Docker volumes...$(RESET)"; \
			docker volume rm $(DATA_VOLUME) $(ENV_VOLUME) 2>/dev/null || true; \
			echo "$(GREEN)Volumes removed!$(RESET)"; \
			;; \
		*) \
			echo "$(BLUE)Volume cleanup cancelled. Data preserved.$(RESET)"; \
			;; \
	esac

clean: clean-image
	@echo "$(RED)Warning: This will delete container, image, and all build data!$(RESET)"
	@echo "$(YELLOW)Volumes to be deleted: $(DATA_VOLUME), $(ENV_VOLUME)$(RESET)"
	@printf "Are you sure? [y/N] "; \
	read answer; \
	case "$$answer" in \
		[Yy]*) \
			echo "$(YELLOW)Removing Docker volumes...$(RESET)"; \
			docker volume rm $(DATA_VOLUME) $(ENV_VOLUME) 2>/dev/null || true; \
			echo "$(GREEN)Complete cleanup done!$(RESET)"; \
			;; \
		*) \
			echo "$(BLUE)Cleanup cancelled. Everything preserved.$(RESET)"; \
			;; \
	esac

.DEFAULT_GOAL := help
