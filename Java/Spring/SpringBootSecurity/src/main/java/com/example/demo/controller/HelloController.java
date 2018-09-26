package com.example.demo.controller;

import com.example.demo.domain.Fruit;
import com.example.demo.mapper1.Fruit1Mapper;
import com.example.demo.mapper2.Fruit2Mapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

@Controller
public class HelloController {

    @Value("${logfile}")
    private String logfile;

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
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        String name = auth.getName(); //get logged in username
        try
        {
            FileWriter fw = new FileWriter(logfile,true); //the true will append the new data
            fw.write(name +  " added a line\n");//appends the string to the file
            fw.close();
            System.out.println("added a line to " + logfile);
        }
        catch(IOException ioe)
        {
            System.err.println("IOException: " + ioe.getMessage());
        }
        return "hello";
    }

}