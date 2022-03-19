package test;

import org.antlr.v4.runtime.*;
import test.gen.CLexer;
import test.gen.CParser;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {
    static String getExt(String name) {
        int pos = name.lastIndexOf('.');
        if (pos<0)
            return "";
        else
            return name.substring(pos);
    }

    static boolean filter_ch(File f) {
        String ext = getExt(f.getName());
        return ext.equals(".c") || ext.equals(".h");
    }

    static List<File> listf(String directoryName) {
        File directory = new File(directoryName);

        List<File> resultList = new ArrayList<File>();

        File[] fList = directory.listFiles();
        for (File file : fList) {
            if (file.isFile()) {
                if (filter_ch(file))
                    resultList.add(file);
            } else if (file.isDirectory()) {
                resultList.addAll(listf(file.getAbsolutePath()));
            }
        }
        return resultList;
    }
    static void parse(String inputFileName) throws IOException {
        System.out.println(inputFileName);
        CharStream codePointCharStream = CharStreams.fromFileName(inputFileName);
        CLexer lexer = new CLexer(codePointCharStream);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CParser parser = new CParser(tokens);
        CParser.CompilationUnitContext tree = parser.compilationUnit();
    }

    public static void main(String[] args) throws IOException {
        List<File> files = listf("examples");
        System.out.printf("%d files in directory",files.size());
        for (File file: files) {
            parse(file.getPath());
        }
    }
}
