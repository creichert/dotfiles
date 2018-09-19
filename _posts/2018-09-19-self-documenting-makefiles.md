---
title: Self-documenting makefiles
description: "Automatically create a help recipe for Makefiles"
tags:
  - make
---


I've been through quite a lot of iterations trying to get makefiles
to automatically print help docs. Recently, I settled on this in
a core `Makefile`


```makefile

.PHONY: help
help: ## Show help & usage dialog.
	@bash -c 'perl -e "$$generate_help_docs" $(MAKEFILE_LIST)'

build: ##@core build the project

foo: ## foo docs
```

Then, at the bottom of the `Makefile`, I put this script. I tried my
hardest to come up w/ something that was a less invasive blob but it make
untenable.

```
export generate_help_docs
define generate_help_docs


# create an array of arrays of all the help docs grouped
# by @ tag

%help;
while(<>) {
    if(/^([a-z0-9-]+):.*\#\#(?:@(\w+))?\s(.*)$$/) {
        push(@{$$help{$$2}}, [$$1, $$3]);
    }
};

use Text::Wrap;

$$right_pad = 30;
$$max_width = 80;

$$Text::Wrap::columns = $$max_width - int($$right_pad/2);


# print the help

print "usage: make [target]\n\n";
print "targets:\n";
foreach ( sort keys %help ) {

    if (length($$_) > 0) {
        print "$$_:\n";
    };

    printf("  \033[36m%-30s\033[0m %s\n%s",

            $$_->[0],
	    wrap('','                                   ', $$_->[1])
          ) for @{$$help{$$_}};

    print "\n";
};

endef
```
