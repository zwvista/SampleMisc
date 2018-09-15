package com.example.demo.mapper1;

import com.example.demo.domain.Fruit;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

@Mapper
public interface Fruit1Mapper {
    List<Fruit> selectAll();
}
