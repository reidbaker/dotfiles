---
name: dependabot-android
description:
  Expertise in updating generated pull requests from dependabot to pass flutter packages presubmits. Use when user mentions updating dependabot, fixing dependabot or fixing dependabot pull requests in flutter/packages. Do not use when user want to fix flutter packages that do not reference dependabot. 
---

# Dependabot android instructions 

## Gather information about the pull request

Github urls take the form of https://github.com/flutter/packages/pull/<pull-request-id>. Where pull-request-id is a number. 

Get information about the pull reqeust by using `gh pr view <pull-request-id> --repo flutter/packages`

For example to find information about the pr https://github.com/flutter/packages/pull/11153 you can call `gh pr view 11153 --repo flutter/packages --json author,body,changedFiles,commits,id,labels,mergeStateStatus,mergeable,number,reviews,state,title,url`   

If the pr is closed or merged there is no more work to do. 

Dependabot prs have a title of the format "[Dependabot]: Bump <dependency> from <past-version> to <current-version> in <location>." 
For example "[dependabot]: Bump com.google.android.gms:play-services-auth from 21.4.0 to 21.5.1 in /packages/google_sign_in/google_sign_in_android/android"

Ignore tree-status failures for the purpose of fixing dependabot prs. 

Find failing tests with `gh pr checks <pull-request-id> --repo flutter/packages --json bucket,completedAt,description,event,link,name,startedAt,state,workflow --jq '.[] | select(.bucket == "fail")'`

To find information about the specific failure take the url listed in "link" for the "Linux repo_check" named failure. 
That link takes the format `"link": "https://cr-buildbucket.appspot.com/build/<build-id>",`

Example `https://cr-buildbucket.appspot.com/build/8687185218015310689` where `` is the <build-id>. 

Then turn the build-id into a <buildNumber> by calling  
`curl https://cr-buildbucket.appspot.com/prpc/buildbucket.v2.Builds/GetBuild --json '{"id":"<build-id>"}'`

Continuing the example that would be 
`curl https://cr-buildbucket.appspot.com/prpc/buildbucket.v2.Builds/GetBuild --json '{"id":"8687185218015310689"}'`

That will return a json strings of the format
```
{
  "id": "8687185218015310689",
  "builder": {
    "project": "flutter",
    "bucket": "try",
    "builder": "Linux repo_checks"
  },
  "number": 22728,
  "createdBy": "user:flutter-dashboard@appspot.gserviceaccount.com",
  "createTime": "2026-03-16T08:59:57.922728497Z",
  "startTime": "2026-03-16T09:00:04.089706018Z",
  "endTime": "2026-03-16T09:05:21.309257222Z",
  "updateTime": "2026-03-16T09:05:21.309257222Z",
  "status": "FAILURE",
  "input": {}
}
```
The value in the "number" field is the <buildNumber>. 

The use the <buildNumber> to get the test logs metadata by running the following command. 

```
curl 'https://cr-buildbucket.appspot.com/prpc/buildbucket.v2.Builds/GetBuild' \
  -H 'accept: application/json' \
  -H 'accept-language: en-US,en;q=0.9,es;q=0.8' \
  -H 'cache-control: no-cache' \
  -H 'content-type: application/json' \
  -H 'origin: https://ci.chromium.org' \
  -H 'pragma: no-cache' \
  -H 'priority: u=1, i' \
  -H 'referer: https://ci.chromium.org/' \
  -H 'sec-ch-ua: "Chromium";v="146", "Not-A.Brand";v="24", "Google Chrome";v="146"' \
  -H 'sec-ch-ua-mobile: ?0' \
  -H 'sec-ch-ua-platform: "macOS"' \
  -H 'sec-fetch-dest: empty' \
  -H 'sec-fetch-mode: cors' \
  -H 'sec-fetch-site: cross-site' \
  -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/146.0.0.0 Safari/537.36' \
  -H 'x-return-encrypted-headers: all' \
  --data-raw '{"builder":{"project":"flutter","bucket":"try","builder":"Linux_android Linux repo_checks master"},"buildNumber":<buildNumber>,"mask":{"fields":"id,builder,builderInfo,number,canceledBy,createdBy,createTime,startTime,endTime,cancelTime,status,statusDetails,summaryMarkdown,output,steps,tags,schedulingTimeout,executionTimeout,gracePeriod,ancestorIds,retriable"}}'
```
Ignore the characters `)]}'` that start the response and treat the rest like json. 

Inspect the result and look for "Tasks failed: CHANGELOG and version validation"

You now know that this is a simple dependabot change. 

To fix a simple dependabot change check out the pull request.
You can check out a pull request by using `gh pr checkout <pull-request-id>`

Using the <location> and <current-version> from the pr title follow the instructions in https://raw.githubusercontent.com/flutter/flutter/refs/heads/master/docs/ecosystem/contributing/README.md for how to update the CHANGELOG.md and pubspec.yaml. 

TODO should this say dart run script/tool/bin/flutter_plugin_tools.dart update-release-info --version=minimal --changelog="Updates Java compatibility version to 17 and minimum supported SDK version to Flutter 3.35/Dart 3.9"  --base-branch=50b804adaa5e17bdf4aa9fba14c0bc1629545313 insetad? 

Commit the changes to CHANGELOG.md and pubspec.yaml with a message in the format "Bump version and add changelog entry for <package>"

Validate the change by running `dart run script/tool/bin/flutter_plugin_tools.dart version-check` 

If version-check passes for <package> inform the user that you have made the changes locally and the pr is ready to be pushed. If version-check fails provide your best reason for why your changes failed as output to the user. 