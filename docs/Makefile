gen:
	lpad-gen

clean:
	rm -rf dist

serve:
	cd dist && python -m SimpleHTTPServer 8000

publish: gen
	s3cmd sync -P --delete-removed dist/* s3://www.lambdapad.io
