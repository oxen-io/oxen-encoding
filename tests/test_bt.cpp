#include <limits>
#include <map>
#include <set>

#include "common.h"

TEST_CASE("bt basic value serialization", "[bt][serialization]") {
    int x = 42;
    std::string x_ = bt_serialize(x);
    REQUIRE(bt_serialize(x) == "i42e");

    int64_t ibig = -8'000'000'000'000'000'000LL;
    uint64_t ubig = 10'000'000'000'000'000'000ULL;
    REQUIRE(bt_serialize(ibig) == "i-8000000000000000000e");
    REQUIRE(bt_serialize(std::numeric_limits<int64_t>::min()) == "i-9223372036854775808e");
    REQUIRE(bt_serialize(ubig) == "i10000000000000000000e");
    REQUIRE(bt_serialize(std::numeric_limits<uint64_t>::max()) == "i18446744073709551615e");

    std::unordered_map<std::string, int> m;
    m["hi"] = 123;
    m["omg"] = -7890;
    m["bye"] = 456;
    m["zap"] = 0;
    // bt values are always sorted:
    REQUIRE(bt_serialize(m) == "d3:byei456e2:hii123e3:omgi-7890e3:zapi0ee");

    // Dict-like list serializes as a dict (and get sorted, as above)
    std::list<std::pair<std::string, std::string>> d{{
            {"c", "x"},
            {"a", "z"},
            {"b", "y"},
    }};
    REQUIRE(bt_serialize(d) == "d1:a1:z1:b1:y1:c1:xe");

    std::vector<std::string> v{{"a", "", "\x00"s, "\x00\x00\x00goo"s}};
    REQUIRE(bt_serialize(v) == "l1:a0:1:\0006:\x00\x00\x00gooe"sv);

    std::array v2 = {"a"sv, ""sv, "\x00"sv, "\x00\x00\x00goo"sv};
    REQUIRE(bt_serialize(v2) == "l1:a0:1:\0006:\x00\x00\x00gooe"sv);
}

TEST_CASE("bt nested value serialization", "[bt][serialization]") {
    std::unordered_map<std::string, std::list<std::map<std::string, std::set<int>>>> x{
            {{"foo", {{{"a", {1, 2, 3}}, {"b", {}}}, {{"c", {4, -5}}}}}, {"bar", {}}}};
    REQUIRE(bt_serialize(x) == "d3:barle3:foold1:ali1ei2ei3ee1:bleed1:cli-5ei4eeeee");
}

TEST_CASE("bt basic value deserialization", "[bt][deserialization]") {
    REQUIRE(bt_deserialize<int>("i42e") == 42);

    int64_t ibig = -8'000'000'000'000'000'000LL;
    uint64_t ubig = 10'000'000'000'000'000'000ULL;
    REQUIRE(bt_deserialize<int64_t>("i-8000000000000000000e") == ibig);
    REQUIRE(bt_deserialize<uint64_t>("i10000000000000000000e") == ubig);
    REQUIRE(bt_deserialize<int64_t>("i-9223372036854775808e") ==
            std::numeric_limits<int64_t>::min());
    REQUIRE(bt_deserialize<uint64_t>("i18446744073709551615e") ==
            std::numeric_limits<uint64_t>::max());
    REQUIRE(bt_deserialize<uint32_t>("i4294967295e") == std::numeric_limits<uint32_t>::max());

    REQUIRE_THROWS(bt_deserialize<int64_t>("i-9223372036854775809e"));
    REQUIRE_THROWS(bt_deserialize<uint64_t>("i-1e"));
    REQUIRE_THROWS(bt_deserialize<uint32_t>("i4294967296e"));

    std::unordered_map<std::string, int> m;
    m["hi"] = 123;
    m["omg"] = -7890;
    m["bye"] = 456;
    m["zap"] = 0;
    // bt values are always sorted:
    REQUIRE(bt_deserialize<std::unordered_map<std::string, int>>("d3:byei456e2:hii123e3:omgi-"
                                                                 "7890e3:zapi0ee") == m);

    // Dict-like list can be used for deserialization
    std::list<std::pair<std::string, std::string>> d{{
            {"a", "z"},
            {"b", "y"},
            {"c", "x"},
    }};
    REQUIRE(bt_deserialize<std::list<std::pair<std::string, std::string>>>("d1:a1:z1:b1:y1:c1:"
                                                                           "xe") == d);

    std::vector<std::string> v{{"a", "", "\x00"s, "\x00\x00\x00goo"s}};
    REQUIRE(bt_deserialize<std::vector<std::string>>("l1:a0:1:\0006:\x00\x00\x00gooe"sv) == v);

    std::vector v2 = {"a"sv, ""sv, "\x00"sv, "\x00\x00\x00goo"sv};
    REQUIRE(bt_deserialize<decltype(v2)>("l1:a0:1:\0006:\x00\x00\x00gooe"sv) == v2);
}

TEST_CASE("bt_value serialization", "[bt][serialization][bt_value]") {
    bt_value dna{42};
    std::string x_ = bt_serialize(dna);
    REQUIRE(bt_serialize(dna) == "i42e");

    bt_value foo{"foo"};
    REQUIRE(bt_serialize(foo) == "3:foo");

    bt_value ibig{-8'000'000'000'000'000'000LL};
    bt_value ubig{10'000'000'000'000'000'000ULL};
    int16_t ismall = -123;
    uint16_t usmall = 123;
    bt_dict nums{
            {"a", 0},
            {"b", -8'000'000'000'000'000'000LL},
            {"c", 10'000'000'000'000'000'000ULL},
            {"d", ismall},
            {"e", usmall},
    };

    REQUIRE(bt_serialize(ibig) == "i-8000000000000000000e");
    REQUIRE(bt_serialize(ubig) == "i10000000000000000000e");
    REQUIRE(bt_serialize(nums) ==
            "d1:ai0e1:bi-8000000000000000000e1:ci10000000000000000000e1:di-123e1:ei123ee");

    // Same as nested test, above, but with bt_* types
    bt_dict x{
            {{"foo",
              bt_list{
                      {bt_dict{{{"a", bt_list{{1, 2, 3}}}, {"b", bt_list{}}}},
                       bt_dict{{{"c", bt_list{{-5, 4}}}}}}}},
             {"bar", bt_list{}}}};
    REQUIRE(bt_serialize(x) == "d3:barle3:foold1:ali1ei2ei3ee1:bleed1:cli-5ei4eeeee");
    std::vector<std::string> v{{"a", "", "\x00"s, "\x00\x00\x00goo"s}};
    REQUIRE(bt_serialize(v) == "l1:a0:1:\0006:\x00\x00\x00gooe"sv);

    std::array v2 = {"a"sv, ""sv, "\x00"sv, "\x00\x00\x00goo"sv};
    REQUIRE(bt_serialize(v2) == "l1:a0:1:\0006:\x00\x00\x00gooe"sv);
}

TEST_CASE("bt_value deserialization", "[bt][deserialization][bt_value]") {
    auto dna1 = bt_deserialize<bt_value>("i42e");
    auto dna2 = bt_deserialize<bt_value>("i-42e");
    REQUIRE(var::get<uint64_t>(dna1) == 42);
    REQUIRE(var::get<int64_t>(dna2) == -42);
    REQUIRE_THROWS(var::get<int64_t>(dna1));
    REQUIRE_THROWS(var::get<uint64_t>(dna2));
    REQUIRE(oxenc::get_int<int>(dna1) == 42);
    REQUIRE(oxenc::get_int<int>(dna2) == -42);
    REQUIRE(oxenc::get_int<unsigned>(dna1) == 42);
    REQUIRE_THROWS(oxenc::get_int<unsigned>(dna2));

    bt_value x = bt_deserialize<bt_value>("d3:barle3:foold1:ali1ei2ei3ee1:bleed1:cli-5ei4eeeee");
    REQUIRE(std::holds_alternative<bt_dict>(x));
    bt_dict& a = var::get<bt_dict>(x);
    REQUIRE(a.count("bar"));
    REQUIRE(a.count("foo"));
    REQUIRE(a.size() == 2);
    bt_list& foo = var::get<bt_list>(a["foo"]);
    REQUIRE(foo.size() == 2);
    bt_dict& foo1 = var::get<bt_dict>(foo.front());
    bt_dict& foo2 = var::get<bt_dict>(foo.back());
    REQUIRE(foo1.size() == 2);
    REQUIRE(foo2.size() == 1);
    bt_list& foo1a = var::get<bt_list>(foo1.at("a"));
    bt_list& foo1b = var::get<bt_list>(foo1.at("b"));
    bt_list& foo2c = var::get<bt_list>(foo2.at("c"));
    std::list<int> foo1a_vals, foo1b_vals, foo2c_vals;
    for (auto& v : foo1a)
        foo1a_vals.push_back(oxenc::get_int<int>(v));
    for (auto& v : foo1b)
        foo1b_vals.push_back(oxenc::get_int<int>(v));
    for (auto& v : foo2c)
        foo2c_vals.push_back(oxenc::get_int<int>(v));
    REQUIRE(foo1a_vals == std::list{{1, 2, 3}});
    REQUIRE(foo1b_vals == std::list<int>{});
    REQUIRE(foo2c_vals == std::list{{-5, 4}});

    REQUIRE(var::get<bt_list>(a.at("bar")).empty());
}

TEST_CASE("bt tuple serialization", "[bt][tuple][serialization]") {
    // Deserializing directly into a tuple:
    std::tuple<int, std::string, std::vector<int>> x{42, "hi", {{1, 2, 3, 4, 5}}};
    REQUIRE(bt_serialize(x) == "li42e2:hili1ei2ei3ei4ei5eee");

    using Y = std::tuple<std::string, std::string, std::unordered_map<std::string, int>>;
    REQUIRE(bt_deserialize<Y>("l5:hello3:omgd1:ai1e1:bi2eee") ==
            Y{"hello", "omg", {{"a", 1}, {"b", 2}}});

    using Z = std::tuple<std::tuple<int, std::string, std::string>, std::pair<int, int>>;
    Z z{{3, "abc", "def"}, {4, 5}};
    REQUIRE(bt_serialize(z) == "lli3e3:abc3:defeli4ei5eee");
    REQUIRE(bt_deserialize<Z>("lli6e3:ghi3:jkleli7ei8eee") == Z{{6, "ghi", "jkl"}, {7, 8}});

    using W = std::pair<std::string, std::pair<int, unsigned>>;
    REQUIRE(bt_serialize(W{"zzzzzzzzzz", {42, 42}}) == "l10:zzzzzzzzzzli42ei42eee");

    REQUIRE_THROWS(bt_deserialize<std::tuple<int>>("li1e"));                  // missing closing e
    REQUIRE_THROWS(bt_deserialize<std::pair<int, int>>("li1ei-4e"));          // missing closing e
    REQUIRE_THROWS(bt_deserialize<std::tuple<int>>("li1ei2ee"));              // too many elements
    REQUIRE_THROWS(bt_deserialize<std::pair<int, int>>("li1ei-2e0:e"));       // too many elements
    REQUIRE_THROWS(bt_deserialize<std::tuple<int, int>>("li1ee"));            // too few elements
    REQUIRE_THROWS(bt_deserialize<std::pair<int, int>>("li1ee"));             // too few elements
    REQUIRE_THROWS(bt_deserialize<std::tuple<std::string>>("li1ee"));         // wrong element type
    REQUIRE_THROWS(bt_deserialize<std::pair<int, std::string>>("li1ei8ee"));  // wrong element type
    REQUIRE_THROWS(bt_deserialize<std::pair<int, std::string>>("l1:x1:xe"));  // wrong element type

    // Converting from a generic bt_value/bt_list:
    bt_value a = bt_get("l5:hello3:omgi12345ee");
    using V1 = std::tuple<std::string, std::string_view, uint16_t>;
    REQUIRE(get_tuple<V1>(a) == V1{"hello", "omg"sv, 12345});

    bt_value b = bt_get("l5:hellod1:ai1e1:bi2eee");
    using V2 = std::pair<std::string_view, bt_dict>;
    REQUIRE(get_tuple<V2>(b) == V2{"hello", {{"a", 1U}, {"b", 2U}}});

    bt_value c = bt_get("l5:helloi-4ed1:ai-1e1:bi-2eee");
    using V3 = std::tuple<std::string, int, bt_dict>;
    REQUIRE(get_tuple<V3>(c) == V3{"hello", -4, {{"a", -1}, {"b", -2}}});

    REQUIRE_THROWS(get_tuple<V1>(bt_get("l5:hello3:omge")));        // too few
    REQUIRE_THROWS(get_tuple<V1>(bt_get("l5:hello3:omgi1ei1ee")));  // too many
    REQUIRE_THROWS(get_tuple<V1>(bt_get("l5:helloi1ei1ee")));       // wrong type

    // Construct a bt_value from tuples:
    bt_value l{std::make_tuple(3, 4, "hi"sv)};
    REQUIRE(bt_serialize(l) == "li3ei4e2:hie");
    bt_list m{{1, 2, std::make_tuple(3, 4, "hi"sv), std::make_pair("foo"s, "bar"sv), -4}};
    REQUIRE(bt_serialize(m) == "li1ei2eli3ei4e2:hiel3:foo3:barei-4ee");
}

TEST_CASE("bt allocation-free consumer", "[bt][dict][list][consumer]") {

    // Consumer deserialization:
    bt_list_consumer lc{"li1ei2eli3ei4e2:hiel3:foo3:barei-4ee"};
    REQUIRE(lc.consume_integer<int>() == 1);
    REQUIRE(lc.consume_integer<int>() == 2);
    REQUIRE(lc.consume_list<std::tuple<int, int, std::string>>() == std::make_tuple(3, 4, "hi"s));
    REQUIRE(lc.consume_list<std::pair<std::string_view, std::string_view>>() ==
            std::make_pair("foo"sv, "bar"sv));
    REQUIRE(lc.consume_integer<int>() == -4);

    bt_dict_consumer dc{"d1:Ai0e1:ali1e3:omge1:bli1ei2ei3eee"};
    REQUIRE(dc.key() == "A");
    REQUIRE(dc.skip_until("a"));
    REQUIRE(dc.next_list<std::pair<int8_t, std::string_view>>() ==
            std::make_pair("a"sv, std::make_pair(int8_t{1}, "omg"sv)));
    REQUIRE(dc.next_list<std::tuple<int, int, int>>() ==
            std::make_pair("b"sv, std::make_tuple(1, 2, 3)));
}

TEST_CASE("bt streaming list producer", "[bt][list][producer]") {

    auto external_buffer = GENERATE(true, false);

    if (external_buffer) {
        char smallbuf[16];
        bt_list_producer toosmall{smallbuf, 16};  // le, total = 2
        toosmall += 42;                           // i42e, total = 6
        toosmall += "abcdefgh";                   // 8:abcdefgh, total=16
        CHECK(toosmall.view() == "li42e8:abcdefghe");

        CHECK_THROWS_AS(toosmall += "", std::length_error);
    }

    char buf[1024];
    auto lp = external_buffer ? bt_list_producer{buf, sizeof(buf)} : bt_list_producer{};
    CHECK(lp.view() == "le");
    size_t orig_cap = 0;
    if (external_buffer)
        CHECK((void*)lp.end() == (void*)(buf + 2));
    else {
        lp.reserve(500);
        orig_cap = lp.str_ref().capacity();
        CHECK(orig_cap >= 500);
    }

    lp.append("abc");
    CHECK(lp.view() == "l3:abce");
    lp += 42;
    CHECK(lp.view() == "l3:abci42ee");
    std::vector<int> randos = {{1, 17, -999}};
    lp.append(randos.begin(), randos.end());
    CHECK(lp.view() == "l3:abci42ei1ei17ei-999ee");

    {
        auto sublist = lp.append_list();
        CHECK_THROWS_AS(lp.append(1), std::logic_error);
        CHECK(sublist.view() == "le");
        CHECK(lp.view() == "l3:abci42ei1ei17ei-999elee");
        sublist.append(0);

        auto sublist2{std::move(sublist)};
        sublist2 += "";
        CHECK(sublist2.view() == "li0e0:e");
        CHECK(lp.view() == "l3:abci42ei1ei17ei-999eli0e0:ee");

        CHECK_THROWS_AS(std::move(sublist2).str(), std::logic_error);
    }

    lp.append_list().append_list().append_list() += "omg"s;
    CHECK(lp.view() == "l3:abci42ei1ei17ei-999eli0e0:elll3:omgeeee");

    {
        auto dict = lp.append_dict();
        CHECK(dict.view() == "de");
        CHECK(lp.view() == "l3:abci42ei1ei17ei-999eli0e0:elll3:omgeeedee");

        CHECK_THROWS_AS(lp.append(1), std::logic_error);

        dict.append("foo", "bar");
        dict.append("g", 42);

        CHECK(dict.view() == "d3:foo3:bar1:gi42ee");
        CHECK(lp.view() == "l3:abci42ei1ei17ei-999eli0e0:elll3:omgeeed3:foo3:bar1:gi42eee");

        dict.append_list("h").append_dict().append_dict("a").append_list("A") += 999;
        CHECK(dict.view() == "d3:foo3:bar1:gi42e1:hld1:ad1:Ali999eeeeee");
        CHECK(lp.view() ==
              "l3:abci42ei1ei17ei-999eli0e0:elll3:omgeeed3:foo3:bar1:gi42e1:hld1:ad1:"
              "Ali999eeeeeee");
    }

    if (external_buffer) {
        CHECK_THROWS_AS(std::move(lp).str(), std::logic_error);
    } else {
        CHECK(orig_cap == lp.str_ref().capacity());
        auto str = std::move(lp).str();
        CHECK(str.capacity() == orig_cap);
        CHECK(str ==
              "l3:abci42ei1ei17ei-999eli0e0:elll3:omgeeed3:foo3:bar1:gi42e1:hld1:ad1:"
              "Ali999eeeeeee");

        CHECK(lp.str_ref().capacity() < 32); // SSO sizes vary across compilers
        CHECK(lp.view() == "le");
    }
}

TEST_CASE("bt streaming dict producer", "[bt][dict][producer]") {

    auto external_buffer = GENERATE(true, false);

    char buf[1024];
    auto dp = external_buffer ? bt_dict_producer{buf, sizeof(buf)} : bt_dict_producer{};
    CHECK(dp.view() == "de");
    size_t orig_cap = 0;
    if (external_buffer)
        CHECK((void*)dp.end() == (void*)(buf + 2));
    else {
        dp.reserve(500);
        orig_cap = dp.str_ref().capacity();
        CHECK(orig_cap >= 500);
    }

    dp.append("foo", "bar");
    CHECK(dp.view() == "d3:foo3:bare");
    dp.append("foo\0"sv, -333222111);
    CHECK(dp.view() == "d3:foo3:bar4:foo\0i-333222111ee"sv);
    {
        auto sublist = dp.append_list("myList");
        ((sublist += "") += 2) += 42;
        CHECK(sublist.view() == "l0:i2ei42ee");
    }
    CHECK(dp.view() == "d3:foo3:bar4:foo\0i-333222111e6:myListl0:i2ei42eee"sv);
    {
        auto subd = dp.append_dict("p");
        subd.append("", "");
        CHECK(subd.view() == "d0:0:e");
    }
    CHECK(dp.view() == "d3:foo3:bar4:foo\0i-333222111e6:myListl0:i2ei42ee1:pd0:0:ee"sv);

    std::map<std::string, int> to_append{{"q", 1}, {"r", 2}, {"~", 3}, {"~1", 4}};
    dp.append(to_append.begin(), to_append.end());

    CHECK(dp.view() ==
          "d3:foo3:bar4:foo\0i-333222111e6:myListl0:i2ei42ee1:pd0:0:e1:qi1e1:ri2e1:~i3e2:~1i4ee"sv);

    if (external_buffer) {
        CHECK_THROWS_AS(std::move(dp).str(), std::logic_error);
    } else {
        CHECK(orig_cap == dp.str_ref().capacity());
        auto str = std::move(dp).str();
        CHECK(str ==
              "d3:foo3:bar4:foo\0i-333222111e6:myListl0:i2ei42ee1:pd0:0:e1:qi1e1:ri2e1:~i3e2:~1i4ee"sv);
        CHECK(str.capacity() == orig_cap);

        CHECK(dp.str_ref().capacity() < 32); // SSO sizes vary across compilers
        CHECK(dp.view() == "de");
    }
}

TEST_CASE("bt_producer/bt_value combo", "[bt][dict][value][producer]") {

    bt_dict_producer x;

    bt_dict more{{"b", 1}, {"c", bt_dict{{"d", "e"}, {"f", bt_list{{1, 2, 3}}}}}};
    bt_dict tiny{{"a", ""}};

    x.append("a", 42);
    x.append_bt("x", bt_value{tiny});
    x.append_bt("y", bt_list{{tiny}});
    x.append_bt("z", more);

    CHECK(x.view() == "d1:ai42e1:xd1:a0:e1:yld1:a0:ee1:zd1:bi1e1:cd1:d1:e1:fli1ei2ei3eeeee");

    bt_list_producer y;
    y.append(123);
    y.append_bt(more);
    y.append_bt(bt_value{tiny});
    y.append_bt(bt_list{{tiny}});
    y.append("~");

    CHECK(y.view() == "li123ed1:bi1e1:cd1:d1:e1:fli1ei2ei3eeeed1:a0:eld1:a0:ee1:~e");
}

#ifdef OXENC_APPLE_TO_CHARS_WORKAROUND
TEST_CASE("apple to_chars workaround test", "[bt][apple][sucks]") {
    char buf[20];
    auto buf_view = [&](char* end) {
        return std::string_view{buf, static_cast<size_t>(end - buf)};
    };
    CHECK(buf_view(oxenc::apple_to_chars10(buf, 0)) == "0");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, 1)) == "1");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, 2)) == "2");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, 10)) == "10");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, 42)) == "42");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, 99)) == "99");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, 1234567890)) == "1234567890");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, -1)) == "-1");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, -2)) == "-2");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, -10)) == "-10");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, -99)) == "-99");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, -1234567890)) == "-1234567890");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, char{42})) == "42");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, (unsigned char){42})) == "42");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, short{42})) == "42");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, std::numeric_limits<char>::min())) == "-128");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, std::numeric_limits<char>::max())) == "127");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, (unsigned char){42})) == "42");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, std::numeric_limits<uint64_t>::max())) ==
          "18446744073709551615");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, int64_t{-1})) == "-1");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, std::numeric_limits<int64_t>::min())) ==
          "-9223372036854775808");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, int64_t{-9223372036854775807})) ==
          "-9223372036854775807");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, int64_t{9223372036854775807})) ==
          "9223372036854775807");
    CHECK(buf_view(oxenc::apple_to_chars10(buf, int64_t{9223372036854775806})) ==
          "9223372036854775806");
}
#endif
