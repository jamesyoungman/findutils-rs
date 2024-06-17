use std::error::Error;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::process::Command;

use tempdir::TempDir;

#[derive(Debug)]
struct TestSetupError(String);

impl Display for TestSetupError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.0.fmt(f)
    }
}

impl From<std::io::Error> for TestSetupError {
    fn from(e: std::io::Error) -> TestSetupError {
        TestSetupError(e.to_string())
    }
}

impl Error for TestSetupError {}

#[cfg(test)]
fn get_binary(name: &str) -> Result<PathBuf, TestSetupError> {
    match std::env::current_exe() {
        Ok(executable) => match executable.parent() {
            Some(exe_dir) => match exe_dir.parent() {
                Some(exe_parent_dir) => {
                    let mut binary = exe_parent_dir.to_path_buf();
                    binary.push(name);
                    Ok(binary)
                }
                None => Err(TestSetupError(format!(
                    "failed to find parent of {}",
                    exe_dir.display()
                ))),
            },
            None => Err(TestSetupError(format!(
                "failed to find parent of {}",
                executable.display()
            ))),
        },
        Err(e) => Err(e.into()),
    }
}

trait TestInput {
    fn create(&self, parent: &Path) -> Result<(), std::io::Error>;
    fn name(&self) -> &Path;
}

#[derive(Debug)]
struct TestEnvironment {
    temp_dir: TempDir,
    find: Command,
    top_level_folders: Vec<Folder>,
    top_level_files: Vec<File>,
}

impl TestEnvironment {
    pub fn new(test_name: &str) -> Result<TestEnvironment, TestSetupError> {
        match TempDir::new(test_name) {
            Err(e) => Err(TestSetupError(format!(
                "failed to create temporary directory for '{test_name}': {e}"
            ))),
            Ok(temp_dir) => {
                let binary_name = "find";
                let find = get_binary(binary_name).map_err(|e| {
                    TestSetupError(format!("failed to find binary {binary_name}: {e}"))
                })?;
                if find.exists() {
                    let mut cmd = Command::new(find);
                    dbg!(&cmd);
                    cmd.current_dir(temp_dir.path());
                    Ok(TestEnvironment {
                        temp_dir,
                        find: cmd,
                        top_level_folders: vec![],
                        top_level_files: vec![],
                    })
                } else {
                    Err(TestSetupError(format!(
                        "binary {} we want to test does not exist",
                        find.display()
                    )))
                }
            }
        }
    }

    fn find(&mut self, args: &[&str]) -> &mut Command {
        if let Err(e) = self.create_files() {
            panic!("Failed to create files for test setup: {e}");
        }
        self.find.args(args)
    }

    fn with_children(&mut self, files: Vec<File>, folders: Vec<Folder>) -> &mut TestEnvironment {
        for name in files
            .iter()
            .map(|file| file.name())
            .chain(folders.iter().map(|folder| folder.name()))
        {
            match name.to_str() {
                Some(name) => {
                    if name.contains('/') {
                        panic!("test file names should not contain '/': {name}");
                    }
                }
                None => {
                    // We could support this we just currently don't.
                    panic!("test file name isn't value utf8");
                }
            }
        }
        self.top_level_files.extend(files.into_iter());
        self.top_level_folders.extend(folders.into_iter());
        self
    }

    fn create_files(&self) -> Result<(), std::io::Error> {
        let path = self.temp_dir.path();
        for folder in self.top_level_folders.iter() {
            folder.create(path)?;
        }
        for file in self.top_level_files.iter() {
            file.create(path)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct File {
    name: PathBuf,
    contents: Vec<u8>,
}

impl From<(&str, &str)> for File {
    fn from((name, contents): (&str, &str)) -> File {
        File {
            name: PathBuf::from(name),
            contents: contents.as_bytes().to_owned(),
        }
    }
}

impl TestInput for File {
    fn create(&self, parent: &Path) -> Result<(), std::io::Error> {
        let full_name: PathBuf = parent.join(&self.name);
        let contents: &[u8] = self.contents.as_ref();
        std::fs::write(&full_name, contents)
    }

    fn name(&self) -> &Path {
        &self.name
    }
}

#[derive(Debug)]
struct Folder {
    name: PathBuf,
    files: Vec<File>,
    subfolders: Vec<Folder>,
}

impl Folder {
    pub fn new(name: &str) -> Folder {
        Folder {
            name: PathBuf::from(name),
            files: vec![],
            subfolders: vec![],
        }
    }

    pub fn new_with_children(name: &str, files: Vec<File>, subfolders: Vec<Folder>) -> Folder {
        Folder {
            name: PathBuf::from(name),
            files,
            subfolders,
        }
    }
}

impl TestInput for Folder {
    fn create(&self, parent: &Path) -> Result<(), std::io::Error> {
        let full_name = parent.join(&self.name);
        std::fs::create_dir(&full_name)?;
        for child in self.files.iter() {
            child.create(&full_name)?;
        }
        for child in self.subfolders.iter() {
            child.create(&full_name)?;
        }
        Ok(())
    }

    fn name(&self) -> &Path {
        &self.name
    }
}

#[should_panic]
#[test]
fn test_environment_invalid_name() {
    match TestEnvironment::new("test_environment_invalid_name") {
        Ok(mut env) => {
            env.with_children(vec![], vec![Folder::new("not/allowed")]);
        }
        Err(e) => {
            // This is a should_panic test, so if we panic here the
            // test will pass.  So to make the test fail, we need to
            // return normally.   But we still need an error message
            // to explain the problem.
            eprintln!("test_environment_invalid_name: test environment could not be created: {e}");
        }
    }
}

#[test]
fn run_find_on_empty_dir() -> Result<(), TestSetupError> {
    let mut env = TestEnvironment::new("run_find_on_empty_dir")?;
    let output = env.find(&[]).output()?;
    assert_eq!(&output.stdout, ".\n".as_bytes());
    Ok(())
}

#[test]
fn run_find_on_single_subdir() -> Result<(), TestSetupError> {
    let mut env = TestEnvironment::new("run_find_on_single_subdir")?;
    let output = env
        .with_children(vec![], vec![Folder::new("only")])
        .find(&[])
        .output()?;
    assert_eq!(
        &String::from_utf8(output.stdout).expect("output should be utf8"),
        ".\n./only\n"
    );
    Ok(())
}

#[test]
fn run_find_mindepth() -> Result<(), TestSetupError> {
    let mut env = TestEnvironment::new("run_find_mindepth")?;
    let output = env
        .with_children(vec![], vec![Folder::new("only")])
        .find(&["-mindepth", "1"])
        .output()?;
    assert_eq!(
        &String::from_utf8(output.stdout).expect("output should be utf8"),
        "./only\n"
    );
    Ok(())
}

#[test]
fn run_find_maxdepth_0() -> Result<(), TestSetupError> {
    let mut env = TestEnvironment::new("run_find_maxdepth_0")?;
    let output = env
        .with_children(vec![], vec![Folder::new("only")])
        .find(&["-maxdepth", "0"])
        .output()?;
    assert_eq!(
        &String::from_utf8(output.stdout).expect("output should be utf8"),
        ".\n"
    );
    Ok(())
}

#[test]
fn run_find_maxdepth_1() -> Result<(), TestSetupError> {
    let mut env = TestEnvironment::new("run_find_maxdepth_1")?;
    let output = env
        .with_children(
            vec![],
            vec![Folder::new_with_children(
                "level1",
                vec![],
                vec![Folder::new("level2")],
            )],
        )
        .find(&["-mindepth", "1", "-maxdepth", "1"])
        .output()?;
    assert_eq!(
        &String::from_utf8(output.stdout).expect("output should be utf8"),
        "./level1\n"
    );
    Ok(())
}

#[test]
fn run_find_true_print() -> Result<(), TestSetupError> {
    let mut env = TestEnvironment::new("run_find_true_print")?;
    let output = env.find(&["-maxdepth", "0", "-true", "-print"]).output()?;
    assert_eq!(
        &String::from_utf8(output.stdout).expect("output should be utf8"),
        ".\n"
    );
    Ok(())
}

#[test]
fn run_find_false_print() -> Result<(), TestSetupError> {
    let mut env = TestEnvironment::new("run_find_false_print")?;
    let output = env.find(&["-maxdepth", "0", "-false", "-print"]).output()?;
    assert_eq!(
        &String::from_utf8(output.stdout).expect("output should be utf8"),
        ""
    );
    Ok(())
}
