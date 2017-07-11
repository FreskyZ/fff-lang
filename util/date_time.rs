///! fff-lang
///!
///! util/date_time for Readonly UTC date time

use std::fmt;

/// Readonly UTC date time
#[derive(Clone, Eq, PartialEq)]
pub struct DateTime {
    m_year: u32,
    m_month: u32, 
    m_day: u32,
    m_hour: u32,
    m_minute: u32,
    m_second: u32,
}
impl DateTime {

    /// Attention: no validate because I'm lazy
    pub fn with6(year: u32, month: u32, day: u32, hour: u32, minute: u32, second: u32) -> DateTime {
        DateTime{ m_year: year, m_month: month, m_day: day, m_hour: hour, m_minute: minute, m_second: second }
    }
    pub fn new() -> DateTime {
        DateTime::with6(1970, 1, 1, 0, 0, 0)
    }
    
    pub fn get_year(&self) -> u32 { self.m_year }
    pub fn get_month(&self) -> u32 { self.m_month }
    pub fn get_day(&self) -> u32 { self.m_day }
    pub fn get_hour(&self) -> u32 { self.m_hour }
    pub fn get_minute(&self) -> u32 { self.m_minute }
    pub fn get_second(&self) -> u32 { self.m_second }

    /// Currently only ISO8601 and UTC
    pub fn format(&self, _formatter : &str) -> String {
        format!("{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z", self.m_year, self.m_month, self.m_day, self.m_hour, self.m_minute, self.m_second)
    }
}
impl fmt::Debug for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format("iso8601"))
    }
}

#[cfg(windows)]
mod native {
    use std::mem;
    use super::DateTime;

    #[repr(C, packed)]
    struct SystemTime {
        year: u16,
        month: u16,
        _day_of_week: u16, // not used here
        day: u16,
        hour: u16,
        minute: u16,
        second: u16,
        _milli_second: u16, // not used here
    }
    #[repr(C, packed)]
    struct FileTime {
        low_part: u32,
        high_part: u32,
    }

    #[link(name = "kernel32")] extern "stdcall" { 
        fn GetSystemTime(retval: *mut SystemTime); 
        fn SystemTimeToFileTime(systime: *const SystemTime, filetime: *mut FileTime);
        fn FileTimeToSystemTime(filetime: *const FileTime, systime: *mut SystemTime);
    }

    const FILETIME_TIMESTAMP_OFFSET: u64 = 11644473600u64;

    pub fn now() -> DateTime {
        unsafe {
            let mut ret_val = mem::uninitialized::<SystemTime>();
            GetSystemTime(&mut ret_val as *mut SystemTime);
            DateTime::with6(
                ret_val.year as u32, ret_val.month as u32, ret_val.day as u32,
                ret_val.hour as u32, ret_val.minute as u32, ret_val.second as u32)
        }
    }
    pub fn with_timestamp(timestamp: u64) -> DateTime {
        unsafe {
            timestamp += FILETIME_TIMESTAMP_OFFSET;
            timestamp *= 10_000_000u64;
            let filetime = FileTime{ low_part: (timestamp & 0xFFFFFFFF) as u32, high_part: (timestamp >> 32) as u32 };
            let mut systime = mem::uninitialized::<SystemTime>();
            FileTimeToSystemTime(&filetime as *const FileTime, &mut systime as *mut SystemTime);
            DateTime::with6(
                systime.year as u32, systime.month as u32, systime.day as u32,
                systime.hour as u32, systime.minute as u32, systime.second as u32)
        }
    }
    pub fn as_timestamp(this: &DateTime) -> u64 {
        unsafe {
            let systime = SystemTime{
                year: this.m_year as u16, month: this.m_month as u16, day: this.m_day as u16,
                hour: this.m_hour as u16, minute: this.m_minute as u16, second: this.m_second as u16,
                _day_of_week: 0, _milli_second: 0
            };
            let mut filetime = mem::uninitialized::<FileTime>();
            SystemTimeToFileTime(&systime as *const SystemTime, &mut filetime as *mut FileTime);
            (filetime.low_part as u64 + ((filetime.high_part as u64) << 32)) / 10_000_000u64 - FILETIME_TIMESTAMP_OFFSET
        }
    }
}
#[cfg(unix)]
mod native {
    use super::DateTime;

    #[repr(C, packed)]
    struct tm {
        second: i32,         // 0-60
        minute: i32,         // 0-59
        hour: i32,           // 0-23
        day_of_month: i32,   // 1-31
        month: i32,          // 0-11
        year: i32,           // 1900+
        _day_of_week: i32,   // Sunday as 0
        _day_of_year: i32,   // 0-365
        _is_daylight_saving_time: i32,
        _gmt_offset: i64,    // second from GMT   // currently only UTC so not used
        _zone: *mut u8,                           // not used, too
    }
    impl tm {
        fn into_datetime(&self) -> DateTime { // because you may not own a struct tm
            DateTime::with6(
                self.year as u32 + 1900, self.month as u32 + 1, self.day_of_month as u32,
                self.hour as u32, self.minute as u32, self.second as u32,
            )
        }
    }

    #[link(name = "c")] extern "cdecl" {
        fn time(stamp: *mut u64) -> u64;
        fn gmtime(stamp: *const u64) -> *const tm;
        fn timegm(t: *mut tm) -> u64;
    }

    pub fn now() -> DateTime {
        unsafe { tm::into_datetime(&*gmtime(&time(0 as *mut u64) as *const u64)) }
    }
    pub fn with_timestamp(timestamp: u64) -> DateTime {
        unsafe { tm::into_datetime(&*gmtime(&timestamp as *const u64)) }
    }
    pub fn as_timestamp(this: &DateTime) -> u64 {
        let mut zone = ['G' as u32 as u8, 'M' as u32 as u8, 'T' as u32 as u8, 0u8]; // C style `"GMT"`, 0 for null terminate
        unsafe { timegm(&mut tm {
            second: this.m_second as i32,
            minute: this.m_minute as i32,
            hour: this.m_hour as i32,
            day_of_month: this.m_day as i32,
            month: this.m_month as i32 - 1,
            year: this.m_year as i32 - 1900,
            _day_of_week: 0,  // mktime says ignored
            _day_of_year: 0,  // mktime says ignored
            _is_daylight_saving_time: -1, // auto
            _gmt_offset: 0,   // only UTC is used here
            _zone: &mut zone[0] as *mut u8,
        } as *mut tm)
    } }
}

impl DateTime {

    pub fn now() -> DateTime { self::native::now() }
    pub fn with_timestamp(timestamp: u64) -> DateTime { self::native::with_timestamp(timestamp) }
    pub fn as_timestamp(&self) -> u64 { self::native::as_timestamp(self) }
}

#[cfg(test)] #[test]
fn date_time_timestamp() {

    assert_eq!(DateTime::with6(2017, 3, 13, 6, 40, 40).as_timestamp(), 1489387240);
    assert_eq!(DateTime::with_timestamp(0), DateTime::new());
    assert_eq!(DateTime::with_timestamp(1489387240), DateTime::with6(2017, 3, 13, 6, 40, 40));
    assert_eq!(DateTime::with_timestamp(12345678).as_timestamp(), 12345678);
    assert_eq!(DateTime::new().format("iso8601"), "1970-01-01T00:00:00Z");
}