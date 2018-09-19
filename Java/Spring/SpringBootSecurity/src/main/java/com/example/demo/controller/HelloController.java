package com.example.demo.controller;

import com.example.demo.domain.Fruit;
import com.example.demo.mapper1.Fruit1Mapper;
import com.example.demo.mapper2.Fruit2Mapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;

@Controller
public class HelloController {

    @Autowired
    Fruit1Mapper fruit1Mapper;
    @Autowired
    Fruit2Mapper fruit2Mapper;

    @RequestMapping("/hello")
    public String index(Model model) {
        List<Fruit> list = fruit1Mapper.selectAll("2018/09/21");
        List<Fruit> list2 = fruit2Mapper.selectAll();
        model.addAttribute("fruits", list);
        model.addAttribute("fruits2", list2);
        return "hello";
    }

}