.assignments: assignments/*.md
	./assignmentify $?
	touch .assignments

assgnmnts: .assignments



#new:
#	@./new_post.sh





watch: assgnmnts
	stack exec blog watch

preview: assgnmnts
	stack exec blog watch

build:  assgnmnts
	stack exec blog build

deploy: build
	stack exec blog deploy

mdeploy: build
	rsync -r _site/* /home/shane/mth410/
	cd /home/shane/mth410
	git add * || true
	git commit -m "Minor" || true
	git push

rebuild: assgnmnts
	stack exec blog rebuild

redeploy: rebuild
	stack exec blog deploy
