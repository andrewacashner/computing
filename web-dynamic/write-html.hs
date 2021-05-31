import Text.XHtml

www = p ! [theclass "basic"] << "Paragraph text"

page = header << thetitle << "Test" +++ body << www

main :: IO ()
main = do
    putStrLn $ prettyHtml page
