#include <iostream>
#include <boost/coroutine2/coroutine.hpp>

int main()
{
    typedef boost::coroutines2::coroutine<int> coro_t;
    int max = 8;
    coro_t::pull_type source(
        [&](coro_t::push_type& sink){
            int first=1,second=1;
            sink(first);
            sink(second);
            for(int i=0;i<max;++i){
                int third=first+second;
                first=second;
                second=third;
                sink(third);
            }
        });

    for(auto i:source)
        std::cout << i <<  " ";
        
    coro_t::push_type sink(
        [&](coro_t::pull_type& source){
            while(source){
                std::cout << source.get() <<  " ";
                source();
            }
        });

    std::vector<int> v{1,1,2,3,5,8,13,21,34,55};
    std::copy(begin(v),end(v),begin(sink));
}

// 1 1 2 3 5 8 13 21 34 55 

