# grease - plan description and runtime

`grease` is a library which provides interfaces to define _plans_ (composed of
_tasks_, which are instances of _procedures_) and execute them. Plans are a
set of futures of outputs. When creating plans, a procedure resolver is required
to make output futures from inputs. The resolver is provided with interfaces to
log, run external commands, schedule concurrent tasks, and persist data.

