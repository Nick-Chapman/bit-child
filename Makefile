
default:
	jbuilder build --dev @DEFAULT

clean:
	rm -rf _build

.PHONY: default clean
