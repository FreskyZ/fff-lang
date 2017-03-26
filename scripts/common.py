import os
import subprocess
import toml

CARGO_BIN = R"C:\Users\Fresk\.cargo\bin\cargo.exe"
GIT_BIN = R"C:\Program Files\Git\bin\git.exe"
SOLUTION_PATH = os.path.abspath(os.path.pardir)

def _get_dir_name(full_dir):
    splitted = full_dir.split(os.path.sep)
    splitted.reverse()
    return splitted[0]

class temp_set_curdir:
    def __init__(self, target_dir):
        self.dir = target_dir
        self.prev_curdir = '.'

    def __enter__(self):
        self.prev_curdir = os.path.abspath(os.curdir)
        os.chdir(self.dir)
        return 0

    def __exit__(self, type, value, traceback):
        os.chdir(self.prev_curdir)

class changing_file:

    def __init__(self, file_name):
        self.file_name = file_name
        self.file_lines = []
    
    def __enter__(self):
        with open(self.file_name, 'r+') as file:
            self.file_lines = file.readlines()
            self.file_lines[len(self.file_lines) - 1] += ' '
        with open(self.file_name, 'w') as file:
            file.writelines(self.file_lines)
        return 0

    def __exit__(self, type, value, traceback):
        last_line = self.file_lines[len(self.file_lines) - 1]
        self.file_lines[len(self.file_lines) - 1] = last_line[:-1]
        with open(self.file_name, 'w') as file:
            file.writelines(self.file_lines)
    
class CargoProject:

    def __init__(self, project_dir):
        self.project_dir = project_dir
        self.project_name = _get_dir_name(project_dir)
        self.cargo_toml_path = os.path.join(project_dir, 'Cargo.toml')
        self.main_file = self._get_main_file()

    def _get_main_file(self):
        with open(self.cargo_toml_path) as cargo_toml_file:
            cargo_toml = toml.loads(cargo_toml_file.read())
            if 'lib' in cargo_toml:
                return os.path.join(self.project_dir, cargo_toml['lib']['path'])
            elif 'bin' in cargo_toml:
                return os.path.join(self.project_dir, cargo_toml['bin'][0]['path'])
            elif 'src' in os.listdir(self.project_dir):
                src = os.path.join(self.project_dir, 'src')
                if 'main.rs' in os.listdir(src):
                    return os.path.join(src, 'main.rs')
                elif 'lib.rs' in os.listdir(src):
                    return os.path.join(src, 'lib.rs')
        return None

    def __str__(self):
        return 'CargoProject \'%s\':\n    location: \'%s\'\n    toml_dir: \'%s\'\n    main_file: \'%s\'' % \
            (self.project_name, self.project_dir, self.cargo_toml_path, self.main_file)

    def set_version(self, version_str):
        lines = []
        with open(self.cargo_toml_path, 'r') as cargo_file:
            lines = cargo_file.readlines()
            lines[2] = 'version = "%s"\n' % version_str        # assume all my cargo files set package version at line 3
        with open(self.cargo_toml_path, 'w') as cargo_file:
            cargo_file.writelines(lines)
            print("[Info] set %s version to %s" % (self.project_name, version_str))
    
    # return retcode, stdout, stderr
    def _call_cargo(self, params, **kwargs):

        enable_log = True
        require_output = False
        if 'log' in kwargs:
            enable_log = kwargs['log']
        if 'require_output' in kwargs:
            require_output = kwargs['require_output']   
        
        with temp_set_curdir(self.project_dir):
            params_desc = ' '.join(params);
            if enable_log:
                print("[Info] calling `cargo %s` on %s, " % (params_desc, self.project_name), end = '')
            process = subprocess.Popen([CARGO_BIN] + params, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
            retcode = process.wait()
            stdout = process.stdout.read().decode(encoding = 'utf-8')
            stderr = process.stderr.read().decode(encoding = 'utf-8')
            if enable_log:
                if retcode != 0:
                    print('get retcode %d' % retcode)
                else:
                    print('succeed')
            if require_output:
                return retcode, stdout, stderr
            else:
                return retcode

    def call_cargo(self, params, **kwargs):
        if type(params) is str:
            return self._call_cargo([params], **kwargs)
        else:
            return self._call_cargo(params, **kwargs)

class CargoSolution:

    def __init__(self, solution_dir):
        self.solution_dir = solution_dir
        self.projects = []
        for fsitem in os.listdir(solution_dir):
            maybe_project_dir = os.path.join(solution_dir, fsitem)
            if os.path.isdir(maybe_project_dir) and 'Cargo.toml' in os.listdir(maybe_project_dir):
                self.projects.append(CargoProject(maybe_project_dir))

    def __str__(self):
        return '\n'.join(map(lambda x: '%s' % x, self.projects))
    
    def __iter__(self):
        return iter(self.projects)
    def __len__(self):
        return len(self.projects)
    def __getitem__(self, index):
        if type(index) is int:
            return self.projects[index]
        elif type(index) is str:
            for project in self.projects:
                if project.project_name == index:
                    return project
            raise KeyError
        else:
            raise TypeError

    def set_version(self, version_str):
        for project in self.projects:
            project.set_version(version_str)

    def call_cargo_all(self, params):
        for project in self.projects:
            project.call_cargo(params)

    def git_current_commit(self):
        process = subprocess.Popen([GIT_BIN, 'rev-parse', 'HEAD'], stdout = subprocess.PIPE)
        process.wait()
        stdout = process.stdout.read().decode(encoding = 'utf-8')
        return stdout

solution = CargoSolution(SOLUTION_PATH)