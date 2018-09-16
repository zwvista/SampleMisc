package com.example.demo.mapper1;

import com.example.demo.domain.Fruit;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@Mapper
public interface Fruit1Mapper {
    List<Fruit> selectAll();
}
