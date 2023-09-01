#pragma once

#include <cassert>
#include <charconv>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>

#include "variant.h"

namespace oxenc {

using namespace std::literals;

class bt_dict_producer;

#if defined(__APPLE__) && defined(__MAC_OS_X_VERSION_MIN_REQUIRED) && \
        __MAC_OS_X_VERSION_MIN_REQUIRED < 101500
#define OXENC_APPLE_TO_CHARS_WORKAROUND
/// Really simplistic version of std::to_chars on Apple, because Apple doesn't allow `std::to_chars`
/// to be used if targetting anything before macOS 10.15.  The buffer must have at least 20 chars of
/// space (for int types up to 64-bit); we return a pointer one past the last char written.
template <typename IntType>
char* apple_to_chars10(char* buf, IntType val) {
    static_assert(std::is_integral_v<IntType> && sizeof(IntType) <= 8);
    if constexpr (std::is_signed_v<IntType>) {
        if (val < 0) {
            buf[0] = '-';
            return apple_to_chars10(buf + 1, static_cast<std::make_unsigned_t<IntType>>(-val));
        }
    }

    // write it to the buffer in reverse (because we don't know how many chars we'll need yet, but
    // writing in reverse will figure that out).
    char* pos = buf;
    do {
        *pos++ = '0' + static_cast<char>(val % 10);
        val /= 10;
    } while (val > 0);

    // Reverse the digits into the right order
    int swaps = (pos - buf) / 2;
    for (int i = 0; i < swaps; i++)
        std::swap(buf[i], pos[-1 - i]);

    return pos;
}
#endif

/// Class that allows you to build a bt-encoded list manually, optionally without copying or
/// allocating memory.  This is essentially the reverse of bt_list_consumer: where it lets you
/// stream-parse a buffer, this class lets you build directly into a buffer.
///
/// Out-of-buffer-space errors throw std::length_error when using an external buffer.
class bt_list_producer {
    friend class bt_dict_producer;

    // For external buffer mode we keep pointers to the start position and past-the-end positions.
    struct buf_span {
        char* const init;
        char* const end;
    };

    // Our output type: either external buffer pointers, or a string that we build:
    using output = std::variant<std::string, buf_span>;

    // Our data for the root list is either a begin/end pointer pair or a string; for
    // sublists it is a pointer to the parent list/dict.
    std::variant<output, bt_list_producer*, bt_dict_producer*> data;

    // Reference to the output; this is simply a reference to the value inside `data` for the
    // root element, and a pointer to the root's value for sublists/subdicts.
    output& out;

    // True indicates we have an open child list/dict
    bool has_child = false;

    // If we have a dict or list parent, this returns a pointer it (for a dict, static cast to
    // the base bt_list_producer type).  nullptr if no parent.
    bt_list_producer* parent();

    // The range that contains this currently serialized value; `from` is the offset (relative
    // to the beginning of the buffer, or beginning of the string) wherever the `l` was written
    // that started this list; `next` is the offset where the next value goes (which will always
    // have a closing `e` in it, to close the list; the `e` gets overwrite upon appending
    // another element).
    const size_t from;
    size_t next{from};

    // Sublist constructors
    explicit bt_list_producer(bt_list_producer* parent, char prefix = 'l');
    explicit bt_list_producer(bt_dict_producer* parent, char prefix = 'l');

    // Internal common constructor for both list and dict producer for external buffer mode.
    bt_list_producer(char* begin, char* end, char prefix);

    // Internal common constructor for both list and dict producer for expandable std::string
    // buffer mode.
    explicit bt_list_producer(char prefix, size_t reserve);

    // Does the actual appending to the buffer, and throwing if we'd overrun.
    void buffer_append(std::string_view d);

    // Appends the 'e's into the buffer to close off open sublists/dicts *without* advancing the
    // buffer position; we do this after each append so that the buffer always contains valid
    // encoded data, even while we are still appending to it, and so that appending something raises
    // a length_error if appending it would not leave enough space for the required e's to close the
    // open list(s)/dict(s).
    void append_intermediate_ends();

    // Writes an integer to the given buffer; returns the one-past-the-data pointer.  Up to 20 bytes
    // will be written and must be available in buf.  Used for both string and integer
    // serialization.
    template <typename IntType>
    char* write_integer(IntType val, char* buf) {
        static_assert(sizeof(IntType) <= 64);

#ifndef OXENC_APPLE_TO_CHARS_WORKAROUND
        auto [ptr, ec] = std::to_chars(buf, buf + 20, val);
        assert(ec == std::errc());
        return ptr;
#else
        // Hate apple.
        return apple_to_chars10(buf, val);
#endif
    }

    // Serializes an integer value and appends it to the output buffer.  Does not call
    // append_intermediate_ends().
    template <typename IntType, std::enable_if_t<std::is_integral_v<IntType>, int> = 0>
    void append_impl(IntType val) {
        char buf[22];  // 'i' + base10 representation + 'e'
        buf[0] = 'i';
        auto* ptr = write_integer(val, buf + 1);
        *ptr++ = 'e';
        buffer_append({buf, static_cast<size_t>(ptr - buf)});
    }

    // Appends a string value, but does not call append_intermediate_ends()
    void append_impl(std::string_view s) {
        char buf[21];  // length + ':'
        auto* ptr = write_integer(s.size(), buf);
        *ptr++ = ':';
        buffer_append({buf, static_cast<size_t>(ptr - buf)});
        buffer_append(s);
    }

  public:
    bt_list_producer(const bt_list_producer&) = delete;
    bt_list_producer& operator=(const bt_list_producer&) = delete;
    bt_list_producer& operator=(bt_list_producer&&) = delete;
    bt_list_producer(bt_list_producer&& other);

    /// Constructs a list producer that writes into the range [begin, end).  If a write would go
    /// beyond the end of the buffer an exception is raised.  Note that this will happen during
    /// construction if the given buffer is not large enough to contain the `le` encoding of an
    /// empty list.
    bt_list_producer(char* begin, char* end) : bt_list_producer{begin, end, 'l'} {}

    /// Constructs a list producer that writes into the range [begin, begin+size).  If a write would
    /// go beyond the end of the buffer an exception is raised.
    bt_list_producer(char* begin, size_t len) : bt_list_producer{begin, begin + len, 'l'} {}

    /// Constructs a list producer that writes to an internal, expandable string.  `reserve` can
    /// be passed a non-zero value to reserve an initial size in the std::string.
    explicit bt_list_producer(size_t reserve = 0) : bt_list_producer{'l', reserve} {}

    ~bt_list_producer();

    /// Returns a string_view into the currently serialized data buffer.  Note that the returned
    /// view includes the `e` list end serialization markers which will be overwritten if the list
    /// (or an active sublist/subdict) is appended to.
    std::string_view view() const {
        if (auto* s = std::get_if<std::string>(&out))
            return std::string_view{*s}.substr(from, next - from + 1);
        auto& b = var::get<buf_span>(out);
        return {b.init + from, static_cast<size_t>(next - from + 1)};
    }

    /// Extracts the string, when not using buffer mode.  This is only usable on the root
    /// list/dict producer, and may only be used in rvalue context, as it destroys the internal
    /// buffer, such as: std::move(producer).str().  Throws logic_error if called on a
    /// sublist/subdict, or on a external buffer producer.
    ///
    /// (If you just want a copy of the string, use `view()` instead).
    std::string str() && {
        if (parent())
            throw std::logic_error{"Cannot call bt_producer .str() on a sublist/subdict"};
        auto* s = std::get_if<std::string>(&out);
        if (!s)
            throw std::logic_error{"Cannot call bt_producer .str() when using an external buffer"};

        std::string ret;
        ret.swap(*s);
        // Leave behind an empty producer
        *s += ret[0];
        *s += 'e';
        next = 1;
        return ret;
    }

    /// Returns the end position in the buffer.  (This is primarily useful for external buffer
    /// mode, but still works in string mode).
    const char* end() const {
        if (auto* s = std::get_if<std::string>(&out))
            return s->data() + next + 1;
        auto* bs = std::get_if<buf_span>(&out);
        assert(bs);
        return bs->init + next + 1;
    }

    /// Appends an element containing binary string data
    void append(std::string_view data) {
        if (has_child)
            throw std::logic_error{"Cannot append to list when a sublist is active"};
        append_impl(data);
        append_intermediate_ends();
    }

    bt_list_producer& operator+=(std::string_view data) {
        append(data);
        return *this;
    }

    /// Appends an integer
    template <typename IntType, std::enable_if_t<std::is_integral_v<IntType>, int> = 0>
    void append(IntType i) {
        if (has_child)
            throw std::logic_error{"Cannot append to list when a sublist is active"};
        append_impl(i);
        append_intermediate_ends();
    }

    template <typename IntType, std::enable_if_t<std::is_integral_v<IntType>, int> = 0>
    bt_list_producer& operator+=(IntType i) {
        append(i);
        return *this;
    }

    /// Appends elements from the range [from, to) to the list.  This does *not* append the elements
    /// as a sublist: for that you should use something like: `l.append_list().append(from, to);`
    template <typename ForwardIt>
    void append(ForwardIt from, ForwardIt to) {
        if (has_child)
            throw std::logic_error{"Cannot append to list when a sublist is active"};
        while (from != to)
            append_impl(*from++);
        append_intermediate_ends();
    }

    /// Appends an optional value: the value will be appended as if by calling `.append(*val)` if
    /// the optional is set, and otherwise (i.e. if given nullopt) nothing is appended.
    template <typename T>
    void append(const std::optional<T>& val) {
        if (val)
            append(*val);
    }

    /// Appends a sublist to this list.  Returns a new bt_list_producer that references the parent
    /// list.  The parent cannot be added to until the sublist is destroyed.  This is meant to be
    /// used via RAII:
    ///
    ///     buf data[16];
    ///     bt_list_producer list{data, sizeof(data)};
    ///     {
    ///         auto sublist = list.append_list();
    ///         sublist.append(42);
    ///     }
    ///     list.append(1);
    ///     // `data` now contains: `lli42eei1ee`
    ///
    /// If doing more complex lifetime management, take care not to allow the child instance to
    /// outlive the parent.
    bt_list_producer append_list();

    /// Appends a dict to this list.  Returns a new bt_dict_producer that references the parent
    /// list.  The parent cannot be added to until the subdict is destroyed.  This is meant to be
    /// used via RAII (see append_list() for details).
    ///
    /// If doing more complex lifetime management, take care not to allow the child instance to
    /// outlive the parent.
    bt_dict_producer append_dict();

    /// Appends a bt_value, bt_dict, or bt_list to this bt_list.  You must include the
    /// bt_value_producer.h header (either directly or via bt.h) to use this method.
    template <typename T>
    void append_bt(const T& bt);
};

/// Class that allows you to build a bt-encoded dict manually, without copying or allocating memory.
/// This is essentially the reverse of bt_dict_consumer: where it lets you stream-parse a buffer,
/// this class lets you build directly into a buffer that you own.
///
/// Note that bt-encoded dicts *must* be produced in (ASCII) ascending key order, but that this is
/// only tracked/enforced for non-release builds (i.e. without -DNDEBUG).
class bt_dict_producer : bt_list_producer {
    friend class bt_list_producer;

    // Subdict constructors

    bt_dict_producer(bt_list_producer* parent) : bt_list_producer{parent, 'd'} {}
    bt_dict_producer(bt_dict_producer* parent) : bt_list_producer{parent, 'd'} {}

    // Checks a just-written key string to make sure it is monotonically increasing from the last
    // key.  Does nothing in a release build.  (The string is outside the defines because otherwise
    // we'd have a ODR violation between debug and non-debug builds).
    std::string last_key;
#ifdef NDEBUG
    constexpr void check_incrementing_key(std::string_view) const {}
#else
    void check_incrementing_key(std::string_view this_key) {
        assert(last_key.empty() || this_key > last_key);
        last_key = this_key;
    }
#endif

  public:
    /// Constructs a dict producer that writes into the range [begin, end).  If a write would go
    /// beyond the end of the buffer an exception is raised.  Note that this will happen during
    /// construction if the given buffer is not large enough to contain the `de` encoding of an
    /// empty list.
    bt_dict_producer(char* begin, char* end) : bt_list_producer{begin, end, 'd'} {}

    /// Constructs a dict producer that writes into the range [begin, begin+size).  If a write would
    /// go beyond the end of the buffer an exception is raised.
    bt_dict_producer(char* begin, size_t len) : bt_list_producer{begin, begin + len, 'd'} {}

    /// Constructs a dict producer that writes to an internal, expandable string.  `reserve` can
    /// be passed a non-zero value to reserve an initial size in the std::string.
    explicit bt_dict_producer(size_t reserve = 0) : bt_list_producer{'d', reserve} {}

    /// Returns a string_view into the currently serialized data buffer.  Note that the returned
    /// view includes the `e` dict end serialization markers which will be overwritten if the dict
    /// (or an active sublist/subdict) is appended to.
    std::string_view view() const {
        return bt_list_producer::view();
    }

    /// Extracts the string, when not using buffer mode.  This is only usable on the root
    /// list/dict producer, and may only be used in rvalue context, as it destroys the internal
    /// buffer, such as: std::move(producer).str().  Throws logic_error if called on a
    /// sublist/subdict, or on a external buffer producer.
    ///
    /// (If you just want a copy of the string, use `view()` instead).
    std::string str() && {
#ifndef NDEBUG
        last_key = {};
#endif
        return std::move(*this).bt_list_producer::str();
    }

    /// Returns the end position in the buffer.
    const char* end() const {
        return bt_list_producer::end();
    }

    /// Appends a key-value pair with a string or integer value.  The key must be > the last key
    /// added, but this is only enforced (with an assertion) in debug builds.
    template <
            typename T,
            std::enable_if_t<
                    std::is_convertible_v<T, std::string_view> || std::is_integral_v<T>,
                    int> = 0>
    void append(std::string_view key, const T& value) {
        if (has_child)
            throw std::logic_error{"Cannot append to list when a sublist is active"};
        check_incrementing_key(key);
        append_impl(key);
        append_impl(value);
        append_intermediate_ends();
    }

    /// Appends a key-value pair with an optional value, *if* the optional is set.  If the value is
    /// nullopt, nothing is appended.
    template <typename T>
    void append(std::string_view key, const std::optional<T>& value) {
        if (value)
            append(key, *value);
    }

    /// Appends pairs from the range [from, to) to the dict.  Elements must have a .first
    /// convertible to a string_view, and a .second that is either string view convertible or an
    /// integer.  This does *not* append the elements as a subdict: for that you should use
    /// something like: `l.append_dict().append(key, from, to);`
    ///
    /// Also note that the range *must* be sorted by keys, which means either using an ordered
    /// container (e.g. std::map) or a manually ordered container (such as a vector or list of
    /// pairs).  unordered_map, however, is not acceptable.
    template <
            typename ForwardIt,
            std::enable_if_t<!std::is_convertible_v<ForwardIt, std::string_view>, int> = 0>
    void append(ForwardIt from, ForwardIt to) {
        if (has_child)
            throw std::logic_error{"Cannot append to list when a sublist is active"};
        using KeyType = std::remove_cv_t<std::decay_t<decltype(from->first)>>;
        using ValType = std::decay_t<decltype(from->second)>;
        static_assert(std::is_convertible_v<decltype(from->first), std::string_view>);
        static_assert(
                std::is_convertible_v<ValType, std::string_view> || std::is_integral_v<ValType>);
        using BadUnorderedMap = std::unordered_map<KeyType, ValType>;
        static_assert(
                !(  // Disallow unordered_map iterators because they are not going to be ordered.
                        std::is_same_v<typename BadUnorderedMap::iterator, ForwardIt> ||
                        std::is_same_v<typename BadUnorderedMap::const_iterator, ForwardIt>));
        while (from != to) {
            const auto& [k, v] = *from++;
            check_incrementing_key(k);
            append_impl(k);
            append_impl(v);
        }
        append_intermediate_ends();
    }

    /// Appends a sub-dict value to this dict with the given key.  Returns a new bt_dict_producer
    /// that references the parent dict.  The parent cannot be added to until the subdict is
    /// destroyed.  Key must be (ascii-comparison) larger than the previous key.
    ///
    /// This is meant to be used via RAII:
    ///
    ///     buf data[32];
    ///     bt_dict_producer dict{data, sizeof(data)};
    ///     {
    ///         auto subdict = dict.begin_dict("myKey");
    ///         subdict.append("x", 42);
    ///     }
    ///     dict.append("y", "");
    ///     // `data` now contains: `d5:myKeyd1:xi42ee1:y0:e`
    ///
    /// If doing more complex lifetime management, take care not to allow the child instance to
    /// outlive the parent.
    bt_dict_producer append_dict(std::string_view key) {
        if (has_child)
            throw std::logic_error{
                    "Cannot call append_dict while another nested list/dict is active"};
        check_incrementing_key(key);
        append_impl(key);
        return bt_dict_producer{this};
    }

    /// Appends a list to this dict with the given key (which must be ascii-larger than the previous
    /// key).  Returns a new bt_list_producer that references the parent dict.  The parent cannot be
    /// added to until the sublist is destroyed.
    ///
    /// This is meant to be used via RAII (see append_dict() for details).
    ///
    /// If doing more complex lifetime management, take care not to allow the child instance to
    /// outlive the parent.
    bt_list_producer append_list(std::string_view key) {
        if (has_child)
            throw std::logic_error{
                    "Cannot call append_list while another nested list/dict is active"};
        check_incrementing_key(key);
        append_impl(key);
        return bt_list_producer{this};
    }

    /// Appends a bt_value, bt_dict, or bt_list to this bt_dict.  You must include the
    /// bt_value_producer.h header (either directly or via bt.h) to use this method.
    template <typename T>
    void append_bt(std::string_view key, const T& bt);
};

inline bt_list_producer::bt_list_producer(bt_list_producer* parent, char prefix) :
        data{parent}, out{parent->out}, from{parent->next} {
    parent->has_child = true;
    buffer_append(std::string_view{&prefix, 1});
    append_intermediate_ends();
    for (; parent; parent = parent->parent())
        parent->next++;
}

inline bt_list_producer::bt_list_producer(bt_dict_producer* parent, char prefix) :
        bt_list_producer{static_cast<bt_list_producer*>(parent), prefix} {
    data = parent;
}

inline bt_list_producer::bt_list_producer(bt_list_producer&& other) :
        data{std::move(other.data)}, out{other.out}, from{other.from}, next{other.next} {
    if (other.has_child)
        throw std::logic_error{"Cannot move bt_list/dict_producer with active sublists/subdicts"};
    var::visit(
            [](auto& x) {
                if constexpr (!std::is_same_v<output&, decltype(x)>)
                    x = nullptr;
            },
            other.data);
}

inline bt_list_producer* bt_list_producer::parent() {
    if (auto* parent = std::get_if<bt_list_producer*>(&data))
        return *parent;
    if (auto* parent = std::get_if<bt_dict_producer*>(&data))
        return static_cast<bt_list_producer*>(*parent);
    return nullptr;
}

inline void bt_list_producer::buffer_append(std::string_view d) {
    if (auto* s = std::get_if<std::string>(&out)) {
        s->resize(next);  // Truncate any trailing e's
        s->append(d);
    } else {
        auto* bs = std::get_if<buf_span>(&out);
        assert(bs);
        size_t avail = std::distance(bs->init + next, bs->end);
        if (d.size() > avail)
            throw std::length_error{"Cannot write bt_producer: buffer size exceeded"};
        std::copy(d.begin(), d.end(), bs->init + next);
    }
    for (auto* p = this; p; p = p->parent())
        p->next += d.size();
}

inline void bt_list_producer::append_intermediate_ends() {
    size_t count = 0;
    for (auto* p = this; p; p = p->parent())
        count++;

    if (auto* s = std::get_if<std::string>(&out))
        s->append(count, 'e');
    else {
        auto* bs = std::get_if<buf_span>(&out);
        assert(bs);
        auto* begin = bs->init + next;
        auto* end = begin + count;
        if (end > bs->end)
            throw std::length_error{"Cannot write bt_producer: buffer size exceeded"};
        std::fill(begin, end, 'e');
    }
}

inline bt_list_producer::~bt_list_producer() {
    auto* p = parent();
    if (!p)
        return;
    assert(!has_child);
    assert(p->has_child);
    p->has_child = false;
}

inline bt_list_producer::bt_list_producer(char* begin, char* end, char prefix) :
        data{buf_span{begin, end}}, out{*std::get_if<output>(&data)}, from{0}, next{0} {
    buffer_append(std::string_view{&prefix, 1});
    append_intermediate_ends();
}

inline bt_list_producer::bt_list_producer(char prefix, size_t reserve) :
        data{std::string{}}, out{*std::get_if<output>(&data)}, from{0}, next{0} {
    if (reserve > 0)
        std::get_if<std::string>(&out)->reserve(reserve);
    buffer_append(std::string_view{&prefix, 1});
    append_intermediate_ends();
}

inline bt_list_producer bt_list_producer::append_list() {
    if (has_child)
        throw std::logic_error{"Cannot call append_list while another nested list/dict is active"};
    return bt_list_producer{this};
}

inline bt_dict_producer bt_list_producer::append_dict() {
    if (has_child)
        throw std::logic_error{"Cannot call append_dict while another nested list/dict is active"};
    return bt_dict_producer{this};
}

}  // namespace oxenc
