# topic assessment loop, note you need prep to run

topic <- 42

plot(prep, "rating", model = stm_fit, method = "continuous", topics = topic)

findThoughts(stm_fit, texts = meta$review_text, topics = topic, n = 10)

