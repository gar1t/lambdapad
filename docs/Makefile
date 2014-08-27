gen:
	lpad-gen

clean:
	rm -rf dist

publish: gen
	s3cmd sync -P --delete-removed dist/* s3://www.lambdapad.io
