all: happening

happening: src/*.lisp *.asd
	/usr/bin/sbcl --eval "(asdf:make :happening)" --quit

# Generate embedded GeoIP data from ipverse repository
# Run this after cloning: git clone https://github.com/ipverse/country-ip-blocks data/country-ip-blocks
generate-geoip:
	@if [ -d "data/country-ip-blocks/country" ]; then \
		echo "Generating embedded GeoIP data..."; \
		sbcl --noinform --non-interactive --load scripts/generate-geoip-data.lisp; \
	else \
		echo "ipverse data not found. Run: git clone https://github.com/ipverse/country-ip-blocks data/country-ip-blocks"; \
		exit 1; \
	fi

clean:
	rm -rf *~ happening

.PHONY: all clean generate-geoip
