/// Clicks **Re-run failed** on the Flutter Presubmit Dashboard for one PR
/// commit, using the signed-in work Chrome profile on macOS.
///
/// Usage: `dart rerun_presubmit.dart <flutter|packages> <pr> <sha>`
///
/// The script finds a Google Chrome window with a `.corp.google.com` tab,
/// opens the dashboard in a new tab there, turns on Flutter web semantics,
/// clicks the enabled `Re-run failed` button, then closes the tab and
/// restores the tab that was active before.
///
/// Exit codes: 0 clicked, 1 osascript or Chrome error, 2 no work Chrome
/// window, 3 button not found, 4 button disabled, 64 bad arguments.
library;

import 'dart:io';

const _usage = 'Usage: dart rerun_presubmit.dart <flutter|packages> <pr> <sha>';
const _dashboardHost = 'flutter-dashboard.appspot.com';
const _pollCount = 10;
const _pollInterval = Duration(seconds: 2);
const _postClickWait = Duration(seconds: 5);

/// A failure that maps to a process exit code.
class RerunException implements Exception {
  RerunException(this.exitCode, this.message);

  final int exitCode;
  final String message;

  @override
  String toString() => message;
}

Future<void> main(List<String> args) async {
  final usageError = validateArgs(args);
  if (usageError != null) {
    stderr
      ..writeln('Error: $usageError')
      ..writeln(_usage);
    exit(64);
  }
  try {
    await rerun(args[0], args[1], args[2]);
  } on RerunException catch (e) {
    stderr.writeln('Error: ${e.message}');
    exit(e.exitCode);
  }
}

/// Returns an error message for bad [args], or null when they are valid.
String? validateArgs(List<String> args) {
  if (args.length != 3) return 'expected 3 arguments, got ${args.length}.';
  final [repo, pr, sha] = args;
  if (repo != 'flutter' && repo != 'packages') {
    return 'repo must be "flutter" or "packages", got "$repo".';
  }
  if (!RegExp(r'^[1-9][0-9]*$').hasMatch(pr)) {
    return 'pr must be a positive number, got "$pr".';
  }
  if (!RegExp(r'^[0-9a-f]{40}$').hasMatch(sha)) {
    return 'sha must be 40 lowercase hex characters, got "$sha".';
  }
  return null;
}

/// Opens the dashboard for [repo] [pr] at [sha] and clicks Re-run failed.
Future<void> rerun(String repo, String pr, String sha) async {
  final url = 'https://$_dashboardHost/#/presubmit?repo=$repo&pr=$pr&sha=$sha';
  final tab = await _openTab(url);
  stdout.writeln(
    'Opened tab win=${tab.winId} tab=${tab.tabId} origIdx=${tab.origIdx}',
  );
  var result = 'NOT_FOUND';
  try {
    result = await _pollAndClick(tab);
    stdout.writeln('Click result: $result');
  } finally {
    final closed = await _closeTab(tab);
    stdout.writeln('Cleanup tab: $closed');
  }
  if (result.startsWith('CLICKED')) return;
  throw switch (result) {
    'DISABLED' => RerunException(
      4,
      'Re-run failed button is disabled. Nothing failed, or a retry is '
      'already running.',
    ),
    'TAB_GONE' => RerunException(1, 'Dashboard tab closed before the click.'),
    _ => RerunException(
      3,
      'Re-run failed button not found after '
      '${_pollCount * _pollInterval.inSeconds}s. Check the work Chrome '
      'profile is signed in to the dashboard.',
    ),
  };
}

typedef _Tab = ({String winId, String tabId, String origIdx});

Future<String> _pollAndClick(_Tab tab) async {
  var result = 'NOT_FOUND';
  for (var i = 0; i < _pollCount; i++) {
    await Future<void>.delayed(_pollInterval);
    result = await _runJs(tab, _clickJs);
    if (result.startsWith('CLICKED')) {
      await Future<void>.delayed(_postClickWait);
      return result;
    }
    if (result == 'TAB_GONE') return result;
  }
  return result;
}

Future<_Tab> _openTab(String url) async {
  final String ids;
  try {
    ids = await _osa(_openTabScript, [url]);
  } on RerunException catch (e) {
    if (e.message.contains('NO_WORK_WINDOW')) {
      throw RerunException(
        2,
        'No Google Chrome window with a .corp.google.com tab. Open the work '
        'Chrome profile and load any corp page.',
      );
    }
    rethrow;
  }
  final parts = ids.split(',');
  if (parts.length != 3) {
    throw RerunException(1, 'Unexpected osascript output: "$ids".');
  }
  return (winId: parts[0], tabId: parts[1], origIdx: parts[2]);
}

Future<String> _runJs(_Tab tab, String js) =>
    _osa(_runJsScript, [tab.winId, tab.tabId, js]);

Future<String> _closeTab(_Tab tab) async {
  try {
    return await _osa(_closeTabScript, [tab.winId, tab.tabId, tab.origIdx]);
  } on RerunException catch (e) {
    return 'FAILED (${e.message})';
  }
}

Future<String> _osa(String script, List<String> args) async {
  final ProcessResult res;
  try {
    res = await Process.run('osascript', ['-e', script, ...args]);
  } on ProcessException catch (e) {
    throw RerunException(1, 'Could not run osascript: ${e.message}');
  }
  if (res.exitCode == 0) return (res.stdout as String).trim();
  final err = (res.stderr as String).trim();
  throw RerunException(
    1,
    'osascript failed (${res.exitCode}): $err${_hint(err)}',
  );
}

String _hint(String err) {
  if (err.contains('-1743') || err.contains('Not authorized')) {
    return '\nHint: allow your terminal to control Google Chrome in System '
        'Settings > Privacy & Security > Automation.';
  }
  if (err.contains('JavaScript')) {
    return '\nHint: in Chrome, turn on View > Developer > Allow JavaScript '
        'from Apple Events.';
  }
  return '';
}

const _openTabScript = '''
on run argv
  set targetUrl to item 1 of argv
  tell application "Google Chrome"
    repeat with w in windows
      repeat with t in tabs of w
        if (URL of t) contains ".corp.google.com" then
          set origIdx to active tab index of w
          set newT to make new tab at end of tabs of w with properties {URL:targetUrl}
          return (id of w as string) & "," & (id of newT as string) & "," & (origIdx as string)
        end if
      end repeat
    end repeat
    error "NO_WORK_WINDOW"
  end tell
end run''';

const _runJsScript =
    '''
on run argv
  set winId to (item 1 of argv) as integer
  set tabId to item 2 of argv
  set jsCode to item 3 of argv
  tell application "Google Chrome"
    set w to window id winId
    repeat with t in tabs of w
      if (id of t as string) is tabId then
        if (URL of t) does not contain "$_dashboardHost" then
          error "URL guard failed: " & (URL of t)
        end if
        return execute t javascript jsCode
      end if
    end repeat
    return "TAB_GONE"
  end tell
end run''';

const _closeTabScript =
    '''
on run argv
  set winId to (item 1 of argv) as integer
  set tabId to item 2 of argv
  set origIdx to (item 3 of argv) as integer
  tell application "Google Chrome"
    set w to window id winId
    repeat with t in tabs of w
      if (id of t as string) is tabId and (URL of t) contains "$_dashboardHost" then
        close t
        if origIdx <= (count of tabs of w) then
          set active tab index of w to origIdx
        end if
        return "CLOSED"
      end if
    end repeat
    return "NOT_FOUND"
  end tell
end run''';

/// Turns on Flutter web semantics, then finds and clicks the button.
///
/// The dashboard is a Flutter web app, so its buttons only exist in the DOM
/// after semantics are on. Clicking `flt-semantics-placeholder` turns them
/// on. Both passes walk open shadow roots.
const _clickJs = r'''
(function() {
  function eachEl(root, fn) {
    if (!root || !root.querySelectorAll) return false;
    var all = root.querySelectorAll('*');
    for (var i = 0; i < all.length; i++) {
      if (fn(all[i])) return true;
      if (all[i].shadowRoot && eachEl(all[i].shadowRoot, fn)) return true;
    }
    return false;
  }
  eachEl(document, function(el) {
    if (el.tagName === 'FLT-SEMANTICS-PLACEHOLDER') { el.focus(); el.click(); }
    return false;
  });
  var found = null;
  eachEl(document, function(el) {
    var lbl = (el.getAttribute('aria-label') || '').trim();
    var isBtn = el.tagName === 'FLT-SEMANTICS' ||
        el.getAttribute('role') === 'button';
    var txt = isBtn ? (el.textContent || '').trim() : '';
    if (lbl === 'Re-run failed' || txt === 'Re-run failed') {
      found = el;
      return true;
    }
    return false;
  });
  if (!found) return 'NOT_FOUND';
  if (found.getAttribute('aria-disabled') === 'true') return 'DISABLED';
  found.click();
  return 'CLICKED tag=' + found.tagName + ' role=' + found.getAttribute('role');
})()
''';
