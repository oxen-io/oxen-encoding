#include <type_traits>

#include "bt_producer.h"
#include "bt_value.h"
#include "variant.h"

/// This header provides the implementations of append_bt(bt_value/bt_list/bt_dict) for
/// bt_serialize.  (It is optional to avoid unnecessary includes when not wanted).

namespace oxenc {

namespace detail {

    template <typename CharT>
    void serialize_list(bt_list_producer<CharT>& out, const bt_list& l);
    template <typename CharT>
    void serialize_dict(bt_dict_producer<CharT>& out, const bt_dict& l);

    template <typename CharT>
    struct dict_appender {
        bt_dict_producer<CharT>& out;
        std::string_view key;
        dict_appender(bt_dict_producer<CharT>& out, std::string_view key) : out{out}, key{key} {}

        void operator()(const bt_dict& d) {
            auto subdict = out.append_dict(key);
            serialize_dict(subdict, d);
        }
        void operator()(const bt_list& l) {
            auto sublist = out.append_list(key);
            serialize_list(sublist, l);
        }
        template <typename T>
        void operator()(const T& other) {
            out.append(key, other);
        }
    };

    template <typename CharT>
    struct list_appender {
        bt_list_producer<CharT>& out;
        explicit list_appender(bt_list_producer<CharT>& out) : out{out} {}

        void operator()(const bt_dict& d) {
            auto subdict = out.append_dict();
            serialize_dict(subdict, d);
        }
        void operator()(const bt_list& l) {
            auto sublist = out.append_list();
            serialize_list(sublist, l);
        }
        template <typename T>
        void operator()(const T& other) {
            out.append(other);
        }
    };

    template <typename CharT>
    void serialize_dict(bt_dict_producer<CharT>& out, const bt_dict& d) {
        for (const auto& [k, v] : d)
            var::visit(dict_appender{out, k}, static_cast<const bt_variant&>(v));
    }

    template <typename CharT>
    inline void serialize_list(bt_list_producer<CharT>& out, const bt_list& l) {
        for (auto& val : l)
            var::visit(list_appender{out}, static_cast<const bt_variant&>(val));
    }
}  // namespace detail

template <> template <>
inline void bt_list_producer<char>::append_bt(const bt_dict& bt) {
    auto subdict = append_dict();
    detail::serialize_dict(subdict, bt);
}

template <> template <>
inline void bt_list_producer<char>::append_bt(const bt_list& bt) {
    auto sublist = append_list();
    detail::serialize_list(sublist, bt);
}

template <> template <>
inline void bt_list_producer<char>::append_bt(const bt_value& bt) {
    var::visit(detail::list_appender{*this}, static_cast<const bt_variant&>(bt));
}

template <> template <>
inline void bt_dict_producer<char>::append_bt(std::string_view key, const bt_dict& bt) {
    auto subdict = append_dict(key);
    detail::serialize_dict(subdict, bt);
}

template <> template <>
inline void bt_dict_producer<char>::append_bt(std::string_view key, const bt_list& bt) {
    auto sublist = append_list(key);
    detail::serialize_list(sublist, bt);
}

template <> template <>
inline void bt_dict_producer<char>::append_bt(std::string_view key, const bt_value& bt) {
    var::visit(detail::dict_appender{*this, key}, static_cast<const bt_variant&>(bt));
}
}  // namespace oxenc
