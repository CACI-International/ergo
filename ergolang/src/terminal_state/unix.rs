struct TerminalState {
    stdin: Option<termios::Termios>,
    stdout: Option<termios::Termios>,
}

static mut STATE: TerminalState = TerminalState {
    stdin: None,
    stdout: None,
};

pub unsafe fn store() {
    let stdin =
        termios::Termios::from_fd(std::os::unix::io::AsRawFd::as_raw_fd(&std::io::stdin())).ok();
    let stdout =
        termios::Termios::from_fd(std::os::unix::io::AsRawFd::as_raw_fd(&std::io::stdout())).ok();
    STATE = TerminalState { stdin, stdout };
}

pub unsafe fn restore() {
    if let Some(settings) = std::mem::take(&mut STATE.stdin) {
        let fd = std::os::unix::io::AsRawFd::as_raw_fd(&std::io::stdin());
        drop(termios::tcsetattr(fd, termios::TCSANOW, &settings));
    }
    if let Some(settings) = std::mem::take(&mut STATE.stdout) {
        let mut out = std::io::stdout();
        let fd = std::os::unix::io::AsRawFd::as_raw_fd(&out);
        drop(termios::tcsetattr(fd, termios::TCSANOW, &settings));
        // Reset colors, show cursor
        print!("\x1b[m\x1b[?25h");
        drop(std::io::Write::flush(&mut out));
    }
}
