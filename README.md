# [scheduler-fp][]

# building
```sh
nix-build
```
# running tests
```sh
nix-shell --run "cabal test"
```

# todos
- [ ] dhall configs
- [ ] deployment scripts
- [ ] document functionality in readme
- [ ] remove annex related code
- [ ] add date numeral to pretty print
- [ ] use phone number as ID
- [ ] split `<say>` nodes into `<gather>` parent
- [ ] organize modules
- [ ] improve cmd matching
- [ ] improve handling for all-day events (recognize and toss)
- [ ] handle DST
- [ ] limit max suspension days
- [ ] task for verifying env vars and config files
- [ ] consider switching to megaparsec
- [ ] announcement command
- [ ] error logging/reporting
- [ ] handle DST and read TZ from config file
- [ ] push env vars (and other?) into State monad
- [ ] only create gauth token when old one expires
- [x] better in-server documentation
- [x] move to sun-sat week
- [x] write alerts for annex location
- [x] user roles
- [x] alert messages filter by corresponding roles
- [x] suspend response should show month
- [x] default suspension days if no number specified
- [x] "commands" (help) command
- [x] test task for who will be msged by nagger
- [x] deployment
- [x] cron tasks
- [x] expose notification tasks
- [x] tasks leave alone suspended users
- [x] fix adding extra contacts on write
- [x] "shifts" should look at next 7 days
- [x] suspensions
- [x] post exlusivly (not if another event exists)
- [x] claim gcal shifts
- [x] fixz correspondance between shift select map and displayed shifts
- [x] implement twilio msg conversation
- [x] capture twilio cookies
- [x] destroying cookies
- [x] set twilio cookies
- [x] api endpoint and msg response
- [x] contacts in yaml
- [x] emergency msg
- [x] daily nag msg
- [x] fix msg formatting



[scheduler-fp]: https://github.com/goosetherumfoodle/scheduler-fp
