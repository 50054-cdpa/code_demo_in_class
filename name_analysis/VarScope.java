class Main {
    public static int main(String[] argv) {
        int x = 1;
        if (argv.length > 0){
            x = 2;
        } else {
            int y = 1;
        }
        System.out.println(y.toString());
        return 1;
    }
}