from common import solution, changing_file
from datetime import datetime
import re
import os

to_be_test = ['lexical', 'syntax', 'driver']

print('[Info] Making performance at commit %s:' % solution.git_current_commit()[:7])

log_file_name = 'build_performance.md'
if not os.path.isfile(log_file_name):
    with open(log_file_name, 'a+') as log_file:
        print('# fff-lang Build Performace Log\n', file = log_file)
log_file = open(log_file_name, 'a')

print('## Build performance report at `' + datetime.now().strftime('%Y/%m/%d %H:%M:%S') + '` at `' + solution.git_current_commit()[:7] + '`\n', file = log_file)

print('<table>\n<tr class=\'table-header\'><td>project name</td><td>build type</td><td>avg build time</td><td>build times</td></tr>', file = log_file)
for project_name in to_be_test:
    project = solution[project_name]
    for index, params in enumerate([['test', '--no-run'], ['build']]):
        build_times = []
        for _ in range(0, 6):
            with changing_file(project.main_file):
                _, _, stderr = project.call_cargo(params, log = True, require_output = True)
                for stderr_line in [line.strip() for line in stderr.split('\n') if line.strip() != '']:
                    print('%s' % stderr_line)
                    if 'Finished' in stderr_line:
                        time_str = re.search(r'[0-9]+\.[0-9]+', stderr_line).group()
                        build_time = float(time_str)
                        print('[Info] Finish build in %.2f seconds' % build_time)
                        build_times.append(build_time)

        build_times = build_times[1:]
        average_build_time = sum(build_times) / len(build_times)
        if index == 0:
            print('    <tr class=\'row-project-start\'><td>%s</td><td>%s</td><td>%.2fs</td><td>%s</td>\n</tr>' % (project.project_name, params[0], average_build_time, ', '.join(map(lambda x: '%.2fs' % x, build_times))), file = log_file)
        else:
            print('    <tr><td>%s</td><td>%s</td><td>%.2fs</td><td>%s</td>\n</tr>' % ('', params[0], average_build_time, ', '.join(map(lambda x: '%.2fs' % x, build_times))), file = log_file)

        print('[Info] Average build time is %.2f seconds\n' % average_build_time)

solution.call_cargo_all('clean')

print('[Info] Finished making perform report')
print('</table>\n', file = log_file)