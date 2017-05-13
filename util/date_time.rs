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

mod system {

    #[repr(C, packed)]
    pub struct SystemTime {
        pub year: u16,
        pub month: u16,
        pub _day_of_week: u16, // not used here
        pub day: u16,
        pub hour: u16,
        pub minute: u16,
        pub second: u16,
        pub _milli_second: u16, // not used here
    }

    #[repr(C, packed)]
    pub struct FileTime {
        pub low_part: u32,
        pub high_part: u32,
    }

    #[link(name = "kernel32")] extern "stdcall" { 
        pub fn GetSystemTime(retval: *mut SystemTime); 
        pub fn SystemTimeToFileTime(systime: *const SystemTime, filetime: *mut FileTime);
        pub fn FileTimeToSystemTime(filetime: *const FileTime, systime: *mut SystemTime);
    }
}
impl DateTime {

    pub fn now() -> DateTime {
        use std::mem;
        use self::system::{ SystemTime, GetSystemTime };

        unsafe {
            let mut ret_val = mem::uninitialized::<SystemTime>();
            GetSystemTime(&mut ret_val as *mut SystemTime);
            DateTime::with6(
                ret_val.year as u32, ret_val.month as u32, ret_val.day as u32,
                ret_val.hour as u32, ret_val.minute as u32, ret_val.second as u32)
        }
    }

    pub fn with_timestamp(mut timestamp: u64) -> DateTime {
        use std::mem;
        use self::system::{ FileTime, SystemTime, FileTimeToSystemTime };

        unsafe {
            timestamp += 11_644_473_600u64;
            timestamp *= 10_000_000u64;
            let filetime = FileTime{ low_part: (timestamp & 0xFFFFFFFF) as u32, high_part: (timestamp >> 32) as u32 };
            let mut systime = mem::uninitialized::<SystemTime>();
            FileTimeToSystemTime(&filetime as *const FileTime, &mut systime as *mut SystemTime);
            DateTime::with6(
                systime.year as u32, systime.month as u32, systime.day as u32,
                systime.hour as u32, systime.minute as u32, systime.second as u32)
        }
    }
    pub fn as_timestamp(&self) -> u64 {
        use std::mem;
        use self::system::{ FileTime, SystemTime, SystemTimeToFileTime };

        unsafe {
            let systime = SystemTime{
                year: self.m_year as u16, month: self.m_month as u16, day: self.m_day as u16,
                hour: self.m_hour as u16, minute: self.m_minute as u16, second: self.m_second as u16,
                _day_of_week: 0, _milli_second: 0
            };
            let mut filetime = mem::uninitialized::<FileTime>();
            SystemTimeToFileTime(&systime as *const SystemTime, &mut filetime as *mut FileTime);
            (filetime.low_part as u64 + ((filetime.high_part as u64) << 32)) / 10_000_000u64 - 11_644_473_600u64
        }
    }

}

impl fmt::Debug for DateTime {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format("iso8601"))
    }
}

#[cfg(test)] #[test]
fn date_time_timestamp() {

    assert_eq!(DateTime::with6(2017, 3, 13, 6, 40, 40).as_timestamp(), 1489387240);
    assert_eq!(DateTime::with_timestamp(0), DateTime::new());
    assert_eq!(DateTime::with_timestamp(1489387240), DateTime::with6(2017, 3, 13, 6, 40, 40));
    assert_eq!(DateTime::with_timestamp(12345678).as_timestamp(), 12345678);
    assert_eq!(DateTime::new().format("iso8601"), "1970-01-01T00:00:00Z");
}