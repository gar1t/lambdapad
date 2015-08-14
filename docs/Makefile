PORT = 8000

gen:
	lpad-gen

clean:
	rm -rf dist

serve:
	cd dist && python -m SimpleHTTPServer $(PORT)

publish: gen
	s3cmd sync -P --delete-removed dist/* s3://www.lambdapad.io
