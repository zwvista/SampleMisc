package com.example.demo.mapper2;

import com.example.demo.domain.Fruit;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

@Mapper
public interface Fruit2Mapper {
    List<Fruit> selectAll();
}
