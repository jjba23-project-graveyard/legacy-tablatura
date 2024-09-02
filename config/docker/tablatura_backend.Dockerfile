# Copyright © 2022 Josep Bigorra
#
# This file is part of Tablatura.
# Tablatura is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
#   either version 3 of the License, or (at your option) any later version.
#
# Tablatura is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
#   without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with Foobar.
# If not, see <https://www.gnu.org/licenses/>.

# Build phase
FROM docker.io/fpco/stack-build as build

RUN mkdir -p /internal-workspace/dist
COPY . /internal-workspace

RUN cd /internal-workspace && stack build
RUN cd /internal-workspace && stack install tablatura --local-bin-path /opt/workspace/dist

# Packaging phase
#
# Prepare Debian environment where binary runs
FROM docker.io/library/debian:testing-slim

RUN mkdir -p /internal-applications/tablatura
ARG BINARY_PATH
WORKDIR /internal-applications/tablatura

RUN apt update && apt install -y \
	ca-certificates \
	libgmp-dev \
	netbase \
	libstdc++6 \
	imagemagick

# Copy binary from build output and required static files, such as templates
COPY --from=build /opt/workspace/dist/tablatura /internal-applications/tablatura/tablatura-bin
COPY static /internal-applications/tablatura/static
COPY config /internal-applications/tablatura/config

# Entrypoint
CMD ["/internal-applications/tablatura/tablatura-bin"]
