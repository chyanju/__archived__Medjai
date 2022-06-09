func main():
    [ap] = 1000; ap++
    [ap] = 0; ap++
    [ap] = [ap - 2] / [ap - 1]; ap++
    ret
end
