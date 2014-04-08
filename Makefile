SUBDIRS=	lib 8to9 ar as nm od readelf

all:
	make TARGET= subdirs

clean:
	make TARGET=clean subdirs

subdirs:
	for d in $(SUBDIRS); do make -C $$d $(TARGET); done
