lengthWords :: String -> [(String, Int)]
lengthWords sentence = [(word, length word) | word <- words sentence]
