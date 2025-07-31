# esios2fd 0.2.0

- Added `esios2resolution()`, which adds binary flags (1/0) to indicate which time resolutions each indicator supports.  
- Introduced three new functions: `esios2df()`, `esios2lfdata()`, and `date2calendar()`.  
- Expanded supported time resolutions beyond the original two (hourly and 10-minute) to include: 1 min, 5 min, 10 min, 15 min, 30 min, 1 hour, 4 hours, and 1 day.  
*Note:* Not all indicators offer every resolution (e.g. 1 min is often unavailable), and some (notably 15 min) may exhibit irregular or zig-zag values in the raw API output.

# esios2fd 0.1.1

- Fixed a bug in `esios2csv()` related to a misnamed `transmute()` argument (`val = val`).  
- Added a `verbose` flag to `esios2indicators()` for optional progress messages.

# esios2fd 0.1.0

**Initial Release**  
First version of the `esios2fd` package, including:  
- `esios2csv()`  
- `esios2fdata()`  
- `esios2lfdata()`  
- `esios2indicators()`


