package br.ufma.ecp;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

public class App {
    public static void saveToFile(String fileName, String output) {
        FileOutputStream outputStream;

        try {
            outputStream = new FileOutputStream(fileName);
            byte[] strToBytes = output.getBytes();
            outputStream.write(strToBytes);
            outputStream.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static String fromFile(File file) {
        byte[] bytes;

        try {
            bytes = Files.readAllBytes(file.toPath());
            String textoDoArquivo = new String(bytes, "UTF-8");
            return textoDoArquivo;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return "";
    }

    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Please provide a single file path argument.");
            System.exit(1);
        }
        File file = new File(args[0]);

        if (!file.exists()) {
            System.err.println("The file doesn't exist.");
            System.exit(1);
        }

        if (file.isDirectory()) {
            for (File f : file.listFiles()) {
                if (f.isFile() && f.getName().endsWith(".jack")) {
                    var inputFileName = f.getAbsolutePath();
                    var pos = inputFileName.indexOf('.');
                    var outputFileName = inputFileName.substring(0, pos) + ".vm";

                    System.out.println("compiling " + inputFileName);
                    var input = fromFile(f);
                    var parse = new Parser(input.getBytes(StandardCharsets.UTF_8));
                    parse.parse();
                    var result = parse.VMOutput();
                    saveToFile(outputFileName, result);
                }
            }
        } else if (file.isFile()) {
            if (!file.getName().endsWith(".jack")) {
                System.err.println("Please provide a file name ending with .jack");
                System.exit(1);
            } else {
                var inputFileName = file.getAbsolutePath();
                var pos = inputFileName.indexOf('.');
                var outputFileName = inputFileName.substring(0, pos) + ".vm";

                System.out.println("compiling " + inputFileName);
                var input = fromFile(file);
                var parse = new Parser(input.getBytes(StandardCharsets.UTF_8));
                parse.parse();
                var result = parse.VMOutput();
                saveToFile(outputFileName, result);
            }
        }
    }
}