package com.example.demo.mapper2;

import com.example.demo.domain.Fruit;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@Mapper
public interface Fruit2Mapper {
    List<Fruit> selectAll();
}
