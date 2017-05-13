using uint32_t = unsigned int;

extern "C" __declspec(dllimport) void* __stdcall GetStdHandle(uint32_t);
extern "C" __declspec(dllimport) int __stdcall WriteConsoleW(void* handle, const void* buffer, uint32_t chars_to_write, uint32_t* chars_written, void* reserved);
extern "C" __declspec(dllimport) void __stdcall ExitProcess(uint32_t exit_code);

int main(int argc, char** argv) {
    auto handle = GetStdHandle(static_cast<uint32_t>(-11));
    // uint32_t chars_written = 0;
    auto ret_val = WriteConsoleW(handle, L"Helloworld", 10, nullptr, nullptr);
    ExitProcess(static_cast<uint32_t>(ret_val));
}