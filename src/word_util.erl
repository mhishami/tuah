-module(word_util).
-author ('Hisham Ismail <mhishami@gmail.com').

-export ([init/0]).
-export ([gen_phrase_name/0]).
-export ([gen_pnr/0]).

-define (WOODS, [
            <<"Alder">>, <<"Ash">>, <<"Balsa">>, <<"Basswood">>, 
            <<"Beech">>, <<"Birch">>, <<"Boxwood">>, <<"Brazilwood">>, 
            <<"Bubinga">>, <<"Butternut">>, <<"Cherry">>, <<"Chestnut">>,
            <<"Cocobolo">>, <<"Ebony">>, <<"Elm">>, <<"Hickory">>, 
            <<"Jelutong">>, <<"Kingwood">>, <<"Lime">>, <<"Mahogany">>,
            <<"Maple">>, <<"Oak">>, <<"Plane">>, <<"Purpleheart">>,
            <<"Rosewood">>, <<"Sycamore">>, <<"Teak">>, <<"Tulipwood">>
    ]).
-define (METALS, [
            <<"Lithium">>, <<"Beryllium">>, <<"Sodium">>, <<"Magnesium">>,
            <<"Aluminum">>, <<"Potassium">>, <<"Calcium">>, <<"Scandium">>, 
            <<"Titanium">>, <<"Vanadium">>, <<"Chromium">>, <<"Manganese">>, 
            <<"Iron">>, <<"Cobalt">>, <<"Nickel">>, <<"Copper">>, 
            <<"Zinc">>, <<"Gallium">>, <<"Rubidium">>, <<"Strontium">>, 
            <<"Yttrium">>, <<"Zirconium">>, <<"Niobium">>, <<"Molybdenum">>, 
            <<"Technetium">>, <<"Ruthenium">>, <<"Rhodium">>, <<"Palladium">>, 
            <<"Silver">>, <<"Cadmium">>, <<"Indium">>, <<"Tin">>, 
            <<"Cesium">>, <<"Barium">>, <<"Lanthanum">>, <<"Cerium">>
    ]).

-define (LAKES, [
            <<"Gowd">>, <<"Kaptai">>, <<"Fewa">>, <<"Rara">>, 
            <<"Rupa">>, <<"Begnas">>, <<"Shey">>, <<"Avalanche">>, 
            <<"Dal">>, <<"Emerald">>, <<"Chandra">>, <<"Nainital">>, 
            <<"Sheshnag">>, <<"Suraj">>, <<"Moriri">>, <<"Tsongmo">>, 
            <<"Chilka">>, <<"Caspian">>, <<"Urmia">>, <<"Gavkhouni">>, 
            <<"Hamun">>, <<"Namak">>, <<"Bakhtegan">>, <<"Maharloo">>, 
            <<"Hamun">>, <<"Zarivar">>, <<"Habbaniyah">>, <<"Milh">>, 
            <<"Tharthar">>, <<"Sawa">>, <<"Alakol">>, <<"Balkhash">>, 
            <<"Chagan">>, <<"Kaindy">>, <<"Sasykkol">>, <<"Tengiz">>
    ]).

-define (CAPS, [
        <<"A">>, <<"B">>, <<"C">>, <<"D">>, <<"E">>, <<"F">>, <<"G">>,
        <<"H">>, <<"I">>, <<"J">>, <<"K">>, <<"L">>, <<"M">>, <<"N">>,
        <<"O">>, <<"P">>, <<"Q">>, <<"R">>, <<"S">>, <<"T">>, <<"U">>,
        <<"V">>, <<"W">>, <<"X">>, <<"Y">>, <<"Z">>
    ]).
-define (LOWS, [
        <<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>, <<"g">>,
        <<"h">>, <<"i">>, <<"j">>, <<"k">>, <<"l">>, <<"m">>, <<"n">>,
        <<"o">>, <<"p">>, <<"q">>, <<"r">>, <<"s">>, <<"t">>, <<"u">>,
        <<"v">>, <<"w">>, <<"x">>, <<"y">>, <<"z">>
    ]).
-define (NUMS, [
        <<"0">>, <<"1">>, <<"2">>, <<"3">>, <<"4">>,
        <<"5">>, <<"6">>, <<"7">>, <<"8">>, <<"9">>
    ]).

-define (wrandom(List), lists:nth(random(length(List)), List)).

init() ->
    % {H, M, S} = erlang:timestamp(),
    % random:seed(H, M, S).
    crypto:rand_seed(crypto:rand_bytes(8)).

random(N) ->
    crypto:rand_uniform(1, N+1).

gen_phrase_name() ->
    W1 = ?wrandom(?WOODS),
    W2 = ?wrandom(?METALS),
    W3 = ?wrandom(?LAKES),
    <<W1/binary, <<"-">>/binary, W2/binary, <<"-">>/binary, W3/binary>>.

gen_pnr() ->
    gen_pnr(random(3), <<>>).

gen_pnr(1, Accu) when size(Accu) < 6 ->
    P = ?wrandom(?CAPS),
    gen_pnr(random(3), <<Accu/binary, P/binary>>);
gen_pnr(2, Accu) when size(Accu) < 6 ->
    P = ?wrandom(?LOWS),
    gen_pnr(random(3), <<Accu/binary, P/binary>>);
gen_pnr(3, Accu) when size(Accu) < 6 ->
    P = ?wrandom(?NUMS),
    gen_pnr(random(3), <<Accu/binary, P/binary>>);
gen_pnr(_, Accu) -> Accu.


